module Poreus.Endpoint
  ( loadAgent
  , loadAllAgents
  , matchEndpoints
  , agentsMatchingVerb
  , agentsMatchingTag
  ) where

import qualified Data.Aeson as A
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Database.SQLite.Simple (Connection, Only (..), query, query_)

import Poreus.Profile (textToJson)
import Poreus.Time (Timestamp)
import Poreus.Types

loadEndpoints :: MonadIO m => Connection -> Alias -> m [Endpoint]
loadEndpoints c alias = liftIO $ do
  rows <-
    query
      c
      "SELECT verb, arg_schema, param_schema, autonomy, description\n\
      \FROM endpoints WHERE agent_alias = ? ORDER BY verb"
      (Only alias)
      :: IO [(Text, Maybe Text, Maybe Text, Text, Maybe Text)]
  pure (map (toEndpoint alias) rows)

toEndpoint
  :: Alias
  -> (Text, Maybe Text, Maybe Text, Text, Maybe Text)
  -> Endpoint
toEndpoint alias (verb, arg, param, autonomy, desc) =
  Endpoint
    { endpointAgent = alias
    , endpointVerb = verb
    , endpointArgSchema = arg >>= textToJson
    , endpointParamSchema = param >>= textToJson
    , endpointAutonomy = case parseAutonomy autonomy of
        Just a -> a
        Nothing -> AutonomyConfirm
    , endpointDescription = desc
    }

loadAgent :: MonadIO m => Connection -> Alias -> m (Maybe Agent)
loadAgent c alias = do
  rows <- liftIO
    ( query
        c
        "SELECT path, summary, tags, registered_at, updated_at\n\
        \FROM agents WHERE alias = ?"
        (Only alias)
        :: IO [(Text, Maybe Text, Maybe Text, Timestamp, Timestamp)]
    )
  case rows of
    [] -> pure Nothing
    (path, summary, tags, regAt, updAt) : _ -> do
      eps <- loadEndpoints c alias
      pure $
        Just
          Agent
            { agentAlias = alias
            , agentPath = path
            , agentSummary = summary
            , agentTags = parseTags tags
            , agentRegisteredAt = regAt
            , agentUpdatedAt = updAt
            , agentEndpoints = eps
            }

parseTags :: Maybe Text -> [Text]
parseTags Nothing = []
parseTags (Just t) =
  case textToJson t of
    Just (A.Array arr) ->
      [s | A.String s <- foldr (:) [] arr]
    _ -> []

loadAllAgents :: MonadIO m => Connection -> m [Agent]
loadAllAgents c = do
  rows <- liftIO (query_ c "SELECT alias FROM agents ORDER BY alias" :: IO [Only Alias])
  fmap concatPresent (mapM (\(Only a) -> loadAgent c a) rows)

-- | All agents that advertise a matching verb (exact string equality).
agentsMatchingVerb :: MonadIO m => Connection -> Text -> m [Agent]
agentsMatchingVerb c verb = do
  rows <- liftIO
    ( query
        c
        "SELECT DISTINCT agent_alias FROM endpoints WHERE verb = ? ORDER BY agent_alias"
        (Only verb)
        :: IO [Only Alias]
    )
  fmap concatPresent (mapM (\(Only a) -> loadAgent c a) rows)

agentsMatchingTag :: MonadIO m => Connection -> Text -> m [Agent]
agentsMatchingTag c tag = do
  all_ <- loadAllAgents c
  pure [a | a <- all_, tag `elem` agentTags a]

matchEndpoints :: Agent -> Text -> [Endpoint]
matchEndpoints a verb = filter (\e -> endpointVerb e == verb) (agentEndpoints a)

concatPresent :: [Maybe a] -> [a]
concatPresent = foldr step []
  where
    step Nothing acc = acc
    step (Just x) acc = x : acc
