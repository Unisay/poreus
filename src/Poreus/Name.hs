module Poreus.Name
  ( -- * Row
    NameRow (..)

    -- * Validation
  , validateName

    -- * Claim / release / retire (REG-3, REG-6, RECV-2)
  , ClaimOutcome (..)
  , claimName
  , releaseName
  , retireName

    -- * Queries
  , getName
  , listNames
  , boundNameOf

    -- * Send-time resolution (SEND-5)
  , resolveName
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as A
import Data.Char (isAsciiLower, isDigit)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection, Only (..), changes, execute, query, query_)
import Database.SQLite.Simple.FromRow (FromRow (..), field)

import Poreus.Effects.SystemInfo (CanSystemInfo)
import Poreus.Effects.Time (CanTime, currentTime)
import Poreus.JSON (textToJson)
import Poreus.Session (getSession, sessionLive)
import Poreus.Time (Timestamp (..))
import Poreus.Types

-- | A name row: the durable identity + profile that outlives sessions
-- (spec §5). `bound_session` is the claim linking one live session to
-- the name; NULL means released — the name and profile stay intact for
-- the next claimant.
data NameRow = NameRow
  { nameName :: !AgentName
  , nameSummary :: !(Maybe Text)
  , nameTags :: ![Text]
  , nameBoundSession :: !(Maybe SessionAddress)
  , nameBoundAt :: !(Maybe Timestamp)
  , nameCreatedAt :: !Timestamp
  , nameProfileUpdatedAt :: !(Maybe Timestamp)
  }
  deriving stock (Show, Eq)

instance FromRow NameRow where
  fromRow = do
    n <- field
    summary <- field
    tagsT <- field
    bound <- field
    boundAt <- field
    createdAt <- field
    profileAt <- field
    pure
      NameRow
        { nameName = n
        , nameSummary = summary
        , nameTags = fromMaybe [] (tagsT >>= textToJson >>= tagsFromJson)
        , nameBoundSession = bound
        , nameBoundAt = boundAt
        , nameCreatedAt = createdAt
        , nameProfileUpdatedAt = profileAt
        }

tagsFromJson :: A.Value -> Maybe [Text]
tagsFromJson = \case
  A.Array xs -> traverse (\case A.String s -> Just s; _ -> Nothing) (foldr (:) [] xs)
  _ -> Nothing

-- | Names are lowercase kebab-case and must not collide with the
-- session-address space (the @s-@ prefix), which is what lets `to`
-- routing distinguish the two forms without a marker.
validateName :: Text -> Either PoreusError AgentName
validateName t
  | T.null t =
      Left (mkErrorWithAction InvalidInput "name must not be empty" "pass a short kebab-case name, e.g. the repo role")
  | isSessionAddressText t =
      Left (mkErrorWithAction InvalidInput ("name must not start with the session-address prefix '" <> sessionAddressPrefix <> "'") "pick a name without the 's-' prefix")
  | not (validKebab t) =
      Left (mkErrorWithAction InvalidInput ("invalid name '" <> t <> "': names are lowercase kebab-case ([a-z0-9] groups separated by single dashes)") "pick a name like 'nixos' or 'my-repo'")
  | otherwise = Right (AgentName t)

validKebab :: Text -> Bool
validKebab t =
  let groups = T.splitOn "-" t
   in all (\g -> not (T.null g) && T.all isKebabChar g) groups
  where
    isKebabChar ch = isAsciiLower ch || isDigit ch

data ClaimOutcome = ClaimOutcome
  { coName :: !AgentName
  , coPreviousHolder :: !(Maybe SessionAddress)
  -- ^ Set when the claim displaced a binding (dead holder, or takeover).
  , coReleased :: !(Maybe AgentName)
  -- ^ A different name this session held and implicitly released
  -- (one name per session).
  }
  deriving stock (Show, Eq)

-- | Claim a name (REG-3 / RECV-2). Resolution: free, or held by a dead
-- session → claimed; held by me → idempotent no-op; held by another
-- live session → refused with identification of the holder, claimable
-- only via explicit takeover. The binding swap is a guarded UPDATE
-- (compare-and-swap on the observed holder), so two concurrent
-- claimants cannot both win. A claim changes only how future posts
-- resolve — no messages, mailboxes, or cursors move.
claimName ::
  (CanTime m, CanSystemInfo m, MonadIO m) =>
  Connection ->
  SessionAddress ->
  Text ->
  -- | takeover
  Bool ->
  m (Either PoreusError ClaimOutcome)
claimName c me rawName takeover =
  case validateName rawName of
    Left e -> pure (Left e)
    Right name -> do
      now <- Timestamp <$> currentTime
      liftIO $
        execute
          c
          "INSERT OR IGNORE INTO names (name, created_at) VALUES (?, ?)"
          (name, now)
      row <- getName c name
      let holder = row >>= nameBoundSession
      case holder of
        Just h | h == me -> Right . ClaimOutcome name Nothing <$> releaseOthers c me name
        Nothing -> swapBinding c me name now Nothing
        Just h -> do
          holderRow <- getSession c h
          live <- maybe (pure False) sessionLive holderRow
          if not live || takeover
            then swapBinding c me name now (Just h)
            else
              pure . Left $
                PoreusError
                  NameHeld
                  ("name '" <> unAgentName name <> "' is bound to live session " <> unSessionAddress h)
                  (Just "pass takeover: true to claim it anyway, or pick a different name")

-- | Guarded binding swap: succeeds only if the binding still is what we
-- observed. On a lost race the claim is refused conservatively.
swapBinding ::
  MonadIO m =>
  Connection ->
  SessionAddress ->
  AgentName ->
  Timestamp ->
  Maybe SessionAddress ->
  m (Either PoreusError ClaimOutcome)
swapBinding c me name now observed = liftIO $ do
  execute
    c
    "UPDATE names SET bound_session = ?, bound_at = ? WHERE name = ? AND bound_session IS ?"
    (me, now, name, observed)
  n <- changes c
  if n == 0
    then
      pure . Left $
        PoreusError
          NameHeld
          ("name '" <> unAgentName name <> "' was claimed concurrently by another session")
          (Just "re-run discover to see the current holder, then retry with takeover: true if appropriate")
    else do
      released <- releaseOthers c me name
      pure (Right (ClaimOutcome name observed released))

-- | One name per session: binding a new name releases any other one
-- this session held. Returns the released name, if any.
releaseOthers :: MonadIO m => Connection -> SessionAddress -> AgentName -> m (Maybe AgentName)
releaseOthers c me keep = liftIO $ do
  others <-
    query
      c
      "SELECT name FROM names WHERE bound_session = ? AND name != ?"
      (me, keep)
  execute
    c
    "UPDATE names SET bound_session = NULL, bound_at = NULL WHERE bound_session = ? AND name != ?"
    (me, keep)
  pure $ case others of
    (Only n : _) -> Just n
    [] -> Nothing

-- | Release whatever name this session holds (POL-4 handoff). The name
-- and its profile stay for the next claimant. Returns the released
-- name, if any.
releaseName :: MonadIO m => Connection -> SessionAddress -> m (Maybe AgentName)
releaseName c me = liftIO $ do
  held <- query c "SELECT name FROM names WHERE bound_session = ?" (Only me)
  execute
    c
    "UPDATE names SET bound_session = NULL, bound_at = NULL WHERE bound_session = ?"
    (Only me)
  pure $ case held of
    (Only n : _) -> Just n
    [] -> Nothing

-- | Retire a name (REG-6): delete it, its profile, and its endpoints
-- (cascade). Message history involving the name is not rewritten.
-- Returns the count of open requests that were addressed to the name
-- (surfaced, not blocking — OQ-5 leaning).
retireName :: MonadIO m => Connection -> Text -> m (Either PoreusError Int)
retireName c rawName = do
  row <- liftIO $ query c "SELECT name FROM names WHERE name = ?" (Only rawName)
  case row of
    [] ->
      pure . Left $
        PoreusError
          UnknownAgent
          ("name '" <> rawName <> "' does not exist")
          (Just "run discover to list known names")
    (Only (_ :: Text) : _) -> liftIO $ do
      counts <-
        query
          c
          "SELECT COUNT(*) FROM messages m\n\
          \WHERE m.to_name = ? AND m.kind = 'request'\n\
          \  AND NOT EXISTS (SELECT 1 FROM messages n\n\
          \                  WHERE n.in_reply_to = m.id AND n.kind = 'notice')"
          (Only rawName)
      let openCount = case counts of
            (Only n : _) -> n
            [] -> 0
      execute c "DELETE FROM names WHERE name = ?" (Only rawName)
      pure (Right openCount)

getName :: MonadIO m => Connection -> AgentName -> m (Maybe NameRow)
getName c name = liftIO $ do
  rows <-
    query
      c
      "SELECT name, summary, tags, bound_session, bound_at, created_at, profile_updated_at\n\
      \FROM names WHERE name = ?"
      (Only name)
  pure $ case rows of
    (r : _) -> Just r
    [] -> Nothing

listNames :: MonadIO m => Connection -> m [NameRow]
listNames c = liftIO $ do
  query_
    c
    "SELECT name, summary, tags, bound_session, bound_at, created_at, profile_updated_at\n\
    \FROM names ORDER BY name"

boundNameOf :: MonadIO m => Connection -> SessionAddress -> m (Maybe AgentName)
boundNameOf c addr = liftIO $ do
  rows <- query c "SELECT name FROM names WHERE bound_session = ?" (Only addr)
  pure $ case rows of
    (Only n : _) -> Just n
    [] -> Nothing

-- | Send-time name resolution (SEND-5): a never-claimed name is
-- rejected (`unknown-recipient`); a claimed name with no live bound
-- session is rejected (`name-unbound`, OQ-12: fail fast, no
-- store-and-forward). Success yields the session currently bound —
-- the one and only delivery key.
resolveName ::
  (CanTime m, CanSystemInfo m, MonadIO m) =>
  Connection ->
  AgentName ->
  m (Either PoreusError SessionAddress)
resolveName c name = do
  row <- getName c name
  case row of
    Nothing ->
      pure . Left $
        PoreusError
          UnknownRecipient
          ("name '" <> unAgentName name <> "' has never been claimed")
          (Just "run discover to list addressable names and sessions")
    Just NameRow{nameBoundSession = Nothing} -> pure (Left (unbound name))
    Just NameRow{nameBoundSession = Just holder} -> do
      holderRow <- getSession c holder
      live <- maybe (pure False) sessionLive holderRow
      pure $
        if live
          then Right holder
          else Left (unbound name)
  where
    unbound n =
      PoreusError
        NameUnbound
        ("name '" <> unAgentName n <> "' is claimed but no live session is bound to it")
        (Just "open a session in the target workspace (it can claim the name), or wait for presence — check discover")
