module Poreus.Name
  ( -- * Row
    NameRow (..)

    -- * Validation
  , validateName

    -- * Claim / release / retire (REG-3, REG-6, RECV-2)
  , ClaimOutcome (..)
  , claimName
  , releaseName
  , RetireOutcome (..)
  , retireName

    -- * Queries
  , getName
  , listNames
  , boundNameOf
  , mailboxesOf

    -- * Send-time resolution (SEND-5)
  , resolveRole

    -- * Role nudges (fail-fast on missing names)
  , suggestRoleName
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as A
import Data.Char (isAsciiLower, isDigit)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection, Only (..), changes, execute, query, query_)
import Database.SQLite.Simple.FromRow (FromRow (..), field)
import System.FilePath ((</>))

import Poreus.Deliver (pendingCount)
import Poreus.Effects.FileSystem (CanFileSystem, doesDirectoryExist, doesFileExist)
import Poreus.Effects.Process (CanProcess)
import Poreus.Effects.SystemInfo (CanSystemInfo)
import Poreus.Effects.Time (CanTime, currentTime)
import Poreus.JSON (textToJson)
import qualified Poreus.Repo as Repo
import Poreus.Session (SessionRow (..), getSession, sessionLive)
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
              -- The holder's address is deliberately not in this text.
              -- A refusal that hands out an address is read as an
              -- invitation to use it (ADR-0017, L5).
              pure . Left $
                PoreusError
                  NameHeld
                  ("role '" <> unAgentName name <> "' is held by a live session" <> holderLabel holderRow)
                  (Just "pass takeover: true to claim it anyway, or pick a different name")

-- | The host's own name for the current holder, when it published one
-- — enough for a human to find the window, and not an address a peer
-- can post to.
holderLabel :: Maybe SessionRow -> Text
holderLabel = \case
  Just SessionRow{sessHostName = Just n} -> " (host session '" <> n <> "')"
  _ -> ""

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

-- | Retire a name (REG-6): delete it, its profile, its endpoints
-- (cascade), and its delivery cursor. Message history involving the
-- name is not rewritten.
--
-- Retiring a role destroys its mailbox, so the call refuses while
-- anything is still queued there — deleting a role is the one moment
-- when undelivered mail can be lost silently, and the sender is not
-- present to notice. `force` proceeds and reports what it discarded,
-- which makes the loss a decision somebody made rather than a side
-- effect. Already-delivered history stays.
retireName ::
  MonadIO m =>
  Connection ->
  Text ->
  -- | force
  Bool ->
  m (Either PoreusError RetireOutcome)
retireName c rawName force = do
  row <- liftIO $ query c "SELECT name FROM names WHERE name = ?" (Only rawName)
  case row of
    [] ->
      pure . Left $
        PoreusError
          UnknownAgent
          ("name '" <> rawName <> "' does not exist")
          (Just "run discover to list known names")
    (Only (_ :: Text) : _) -> do
      let box = MailboxRole (AgentName rawName)
      queued <- pendingCount c box
      if queued > 0 && not force
        then
          pure . Left $
            PoreusError
              InvalidInput
              ( "role '"
                  <> rawName
                  <> "' still has "
                  <> T.pack (show queued)
                  <> " undelivered message(s) in its mailbox"
              )
              (Just "let a session claim the role and drain it, or pass force: true to retire and discard them")
        else liftIO $ do
          openCount <- openRequestsFor c rawName
          floor_ <- lastSeqOf c rawName
          execute
            c
            "DELETE FROM messages WHERE to_mailbox = ? AND to_kind = 'role' AND seq > ?"
            (rawName, floor_)
          discarded <- changes c
          execute c "DELETE FROM cursors WHERE mailbox = ?" (Only rawName)
          execute c "DELETE FROM names WHERE name = ?" (Only rawName)
          pure (Right (RetireOutcome openCount discarded))

-- | What a retire actually did: how many requests to the role were
-- still unanswered (surfaced, never blocking — OQ-5 leaning), and how
-- many undelivered messages `force` threw away.
data RetireOutcome = RetireOutcome
  { roOpenRequests :: !Int
  , roDiscarded :: !Int
  }
  deriving stock (Show, Eq)

openRequestsFor :: Connection -> Text -> IO Int
openRequestsFor c rawName = do
  counts <-
    query
      c
      "SELECT COUNT(*) FROM messages m\n\
      \WHERE m.to_mailbox = ? AND m.to_kind = 'role' AND m.kind = 'request'\n\
      \  AND NOT EXISTS (SELECT 1 FROM messages n\n\
      \                  WHERE n.in_reply_to = m.id AND n.kind = 'notice')"
      (Only rawName)
  pure $ case counts of
    (Only n : _) -> n
    [] -> 0

lastSeqOf :: Connection -> Text -> IO Int
lastSeqOf c rawName = do
  rows <- query c "SELECT last_seq FROM cursors WHERE mailbox = ?" (Only rawName)
  pure $ case rows of
    (Only n : _) -> n
    [] -> 0

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

-- | Send-time role resolution (SEND-5), reversed from ADR-0012 by
-- ADR-0017.
--
-- Note [Known roles queue, unknown names fail]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A role that exists takes mail whether or not a session is serving
-- it. That is the whole point of a durable mailbox: a peer restarting
-- is not an error condition for the sender.
--
-- A name that was never claimed still fails, because a typo would
-- otherwise create a mailbox nobody will ever drain, and the sender
-- would wait forever on a request that reached no one. `create` lets a
-- sender queue for a role deliberately — seeding work for a session
-- that does not exist yet — so the choice belongs to the sender rather
-- than to poreus guessing.
--
-- Neither failure nor warning ever names a session address. A
-- `name-unbound` message that pointed at an address taught one peer to
-- address sessions directly, and two days later that habit misrouted a
-- real request to a session that merely shared the workspace
-- (ADR-0017, L5/L6).
resolveRole ::
  (CanTime m, MonadIO m) =>
  Connection ->
  AgentName ->
  -- | create the role when it does not exist
  Bool ->
  m (Either PoreusError (Mailbox, [Warning]))
resolveRole c name create = do
  row <- getName c name
  case row of
    Nothing
      | create -> do
          now <- Timestamp <$> currentTime
          liftIO $
            execute
              c
              "INSERT OR IGNORE INTO names (name, created_at) VALUES (?, ?)"
              (name, now)
          pure (Right (MailboxRole name, [created]))
      | otherwise ->
          pure . Left $
            PoreusError
              UnknownRecipient
              ("role '" <> unAgentName name <> "' has never been claimed")
              (Just "run discover to list known roles; pass create_role: true only if you mean to queue work for a role that does not exist yet")
    Just NameRow{nameBoundSession = Nothing} -> pure (Right (MailboxRole name, [unheld]))
    Just NameRow{nameBoundSession = Just _} -> pure (Right (MailboxRole name, []))
  where
    unheld =
      Warning
        "role-unheld"
        ( "no session currently holds the role '"
            <> unAgentName name
            <> "'; the message is queued in the role's mailbox and is delivered when a session claims it"
        )
    created =
      Warning
        "role-created"
        ( "role '"
            <> unAgentName name
            <> "' did not exist and was created by this post; nothing drains its mailbox until a session claims the name"
        )

-- | The mailboxes one session drains: always its own, plus the
-- mailbox of the role it currently holds.
mailboxesOf :: MonadIO m => Connection -> SessionAddress -> m [Mailbox]
mailboxesOf c me = do
  mname <- boundNameOf c me
  pure (MailboxSession me : [MailboxRole n | Just n <- [mname]])

-- | REG-3 nudge support: the workspace-derived name this session could
-- claim right now — Nothing when it already holds one, when the
-- workspace is not a git repository root, when the derived name is
-- invalid, or when another live session holds the role (the
-- parallel-topic-session case: someone else is the front desk, and
-- being nameless is fine). The system never claims on its own; callers
-- surface this as a suggestion and the model/user decides.
suggestRoleName ::
  (CanTime m, CanSystemInfo m, CanFileSystem m, CanProcess m, MonadIO m) =>
  Connection ->
  SessionAddress ->
  FilePath ->
  m (Maybe AgentName)
suggestRoleName c me workspace = do
  bound <- boundNameOf c me
  case bound of
    Just _ -> pure Nothing
    Nothing -> do
      gitDir <- doesDirectoryExist (workspace </> ".git")
      gitFile <- doesFileExist (workspace </> ".git")
      if not (gitDir || gitFile)
        then pure Nothing
        else do
          raw <- Repo.repoAlias workspace
          case validateName raw of
            Left _ -> pure Nothing
            Right nm -> do
              claimable <- nameClaimable c nm
              pure (if claimable then Just nm else Nothing)

-- | Free, released, or held by a dead session — i.e. a claim would
-- succeed without takeover.
nameClaimable ::
  (CanTime m, CanSystemInfo m, MonadIO m) =>
  Connection ->
  AgentName ->
  m Bool
nameClaimable c nm = do
  row <- getName c nm
  case row >>= nameBoundSession of
    Nothing -> pure True
    Just holder -> do
      hr <- getSession c holder
      live <- maybe (pure False) sessionLive hr
      pure (not live)
