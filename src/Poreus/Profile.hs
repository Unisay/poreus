module Poreus.Profile
  ( -- * Endpoints
    EndpointInput (..)
  , EndpointRow (..)

    -- * Publish (REG-4)
  , PublishResult (..)
  , publishProfile
  , validateEndpoints

    -- * Queries
  , endpointsOf
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Char (isAsciiLower, isDigit)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection, Only (..), execute, query)
import Database.SQLite.Simple.FromRow (FromRow (..), field)

import Poreus.DB (withImmediateTransaction)
import Poreus.Effects.Env (CanEnv)
import Poreus.Effects.FileSystem (CanFileSystem)
import Poreus.Effects.SystemInfo (CanSystemInfo)
import Poreus.Effects.Time (CanTime, currentTime)
import Poreus.JSON (jsonToText)
import Poreus.Name (ClaimOutcome (..), boundNameOf, claimName, validateName)
import Poreus.Time (Timestamp (..))
import Poreus.Types

-- | One endpoint as submitted by the profiling agent (spec §5,
-- simplification C: at most a free-text usage hint, no schemas).
data EndpointInput = EndpointInput
  { eiVerb :: !Text
  , eiDescription :: !Text
  , eiAutonomy :: !Autonomy
  , eiUsageHint :: !(Maybe Text)
  }
  deriving stock (Show, Eq)

-- | One endpoint as stored: a typed capability a named agent offers.
data EndpointRow = EndpointRow
  { epName :: !AgentName
  , epVerb :: !Text
  , epDescription :: !Text
  , epAutonomy :: !Autonomy
  , epUsageHint :: !(Maybe Text)
  }
  deriving stock (Show, Eq)

instance FromRow EndpointRow where
  fromRow = do
    n <- field
    verb <- field
    description <- field
    autonomyT <- field
    hint <- field
    pure
      EndpointRow
        { epName = n
        , epVerb = verb
        , epDescription = description
        , epAutonomy = fromMaybe AutonomyConfirm (parseAutonomy autonomyT)
        , epUsageHint = hint
        }

instance ToJSON EndpointRow where
  toJSON e =
    object $
      [ "verb" .= epVerb e
      , "description" .= epDescription e
      , "autonomy" .= epAutonomy e
      ]
        <> maybe [] (\h -> ["usage_hint" .= h]) (epUsageHint e)

data PublishResult = PublishResult
  { prName :: !AgentName
  , prEndpointCount :: !Int
  , prUpdatedAt :: !Timestamp
  , prPreviousHolder :: !(Maybe SessionAddress)
  , prReleased :: !(Maybe AgentName)
  }
  deriving stock (Show, Eq)

-- | Structural endpoint validation (REG-4): kebab-case verbs, unique
-- per profile. Autonomy is enum-checked at the schema layer; anything
-- reaching here is already parsed.
validateEndpoints :: [EndpointInput] -> Either PoreusError ()
validateEndpoints eps = do
  mapM_ checkVerb eps
  checkUnique
  where
    checkVerb e = case validateVerb (eiVerb e) of
      Left err -> Left err
      Right () -> Right ()
    checkUnique =
      let dups =
            [ v
            | (v, n) <- Map.toList (Map.fromListWith (+) [(eiVerb e, 1 :: Int) | e <- eps])
            , n > 1
            ]
       in case dups of
            [] -> Right ()
            (v : _) ->
              Left
                ( mkErrorWithAction
                    InvalidInput
                    ("duplicate endpoint verb '" <> v <> "'")
                    "each verb must appear at most once per profile"
                )

validateVerb :: Text -> Either PoreusError ()
validateVerb v
  | T.null v = Left (mkError InvalidInput "endpoint verb must not be empty")
  | validKebab v = Right ()
  | otherwise =
      Left
        ( mkErrorWithAction
            InvalidInput
            ("invalid endpoint verb '" <> v <> "': verbs are lowercase kebab-case")
            "use a verb like 'deploy-poreus' or 'run-tests'"
        )
  where
    validKebab t = all groupOk (T.splitOn "-" t)
    groupOk g = not (T.null g) && T.all (\ch -> isAsciiLower ch || isDigit ch) g

-- | Publish a capability profile (REG-4): atomically replace the
-- summary, tags, and full endpoint set attached to a name. Publishing
-- implies claiming the name (REG-3) when not yet held; a name held by
-- another live session refuses with `name-held` (no implicit takeover).
publishProfile ::
  (CanTime m, CanSystemInfo m, CanEnv m, CanFileSystem m, MonadIO m) =>
  Connection ->
  SessionAddress ->
  -- | name; defaults to the session's bound name
  Maybe Text ->
  -- | summary
  Text ->
  -- | tags
  [Text] ->
  [EndpointInput] ->
  m (Either PoreusError PublishResult)
publishProfile c me mname summary tags endpoints =
  case validateEndpoints endpoints of
    Left e -> pure (Left e)
    Right () -> do
      targetName <- case mname of
        Just n -> pure (validateName n)
        Nothing -> do
          bound <- boundNameOf c me
          pure $ case bound of
            Just n -> Right n
            Nothing ->
              Left
                ( mkErrorWithAction
                    InvalidInput
                    "no name given and this session holds none"
                    "pass a name, or claim one first with claim_name"
                )
      case targetName of
        Left e -> pure (Left e)
        Right name -> do
          claimed <- claimName c me (unAgentName name) False
          case claimed of
            Left e -> pure (Left e)
            Right outcome -> do
              now <- Timestamp <$> currentTime
              liftIO . withImmediateTransaction c $ do
                execute
                  c
                  "UPDATE names SET summary = ?, tags = ?, profile_updated_at = ? WHERE name = ?"
                  (summary, jsonToText tags, now, name)
                execute c "DELETE FROM endpoints WHERE name = ?" (Only name)
                mapM_
                  ( \e ->
                      execute
                        c
                        "INSERT INTO endpoints (name, verb, description, autonomy, usage_hint)\n\
                        \VALUES (?, ?, ?, ?, ?)"
                        (name, eiVerb e, eiDescription e, autonomyText (eiAutonomy e), eiUsageHint e)
                  )
                  endpoints
              pure . Right $
                PublishResult
                  { prName = name
                  , prEndpointCount = length endpoints
                  , prUpdatedAt = now
                  , prPreviousHolder = coPreviousHolder outcome
                  , prReleased = coReleased outcome
                  }

endpointsOf :: MonadIO m => Connection -> AgentName -> m [EndpointRow]
endpointsOf c name = liftIO $ do
  rows <-
    query
      c
      "SELECT name, verb, description, autonomy, usage_hint FROM endpoints WHERE name = ?"
      (Only name)
  pure (sortOn epVerb rows)
