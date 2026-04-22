module Poreus.Profile
  ( ProfileInput (..)
  , EndpointInput (..)
  , registerAgent
  , putProfile
  , jsonToText
  , textToJson
  ) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Database.SQLite.Simple (Connection, Only (..), execute, query, withTransaction)

import Poreus.Time (Timestamp)
import Poreus.Types

data ProfileInput = ProfileInput
  { piSummary :: !(Maybe Text)
  , piTags :: ![Text]
  , piEndpoints :: ![EndpointInput]
  }
  deriving stock (Show, Eq)

data EndpointInput = EndpointInput
  { eiVerb :: !Text
  , eiArgSchema :: !(Maybe A.Value)
  , eiParamSchema :: !(Maybe A.Value)
  , eiAutonomy :: !Autonomy
  , eiDescription :: !(Maybe Text)
  }
  deriving stock (Show, Eq)

instance A.FromJSON ProfileInput where
  parseJSON = A.withObject "ProfileInput" $ \o ->
    ProfileInput
      <$> o A..:? "summary"
      <*> (o A..:? "tags" A..!= [])
      <*> (o A..:? "endpoints" A..!= [])

instance A.FromJSON EndpointInput where
  parseJSON = A.withObject "EndpointInput" $ \o ->
    EndpointInput
      <$> o A..: "verb"
      <*> o A..:? "arg_schema"
      <*> o A..:? "param_schema"
      <*> o A..: "autonomy"
      <*> o A..:? "description"

-- | UTF-8 JSON text for storage in TEXT columns.
jsonToText :: A.ToJSON a => a -> Text
jsonToText = TE.decodeUtf8 . BL.toStrict . A.encode

-- | Decode a TEXT column back to a JSON Value. Returns Nothing on parse
-- error so callers can fall back gracefully.
textToJson :: Text -> Maybe A.Value
textToJson = A.decodeStrict' . TE.encodeUtf8

-- | Upsert an agent row. Summary/tags are left untouched on update.
-- `now` is supplied by the caller so tests drive the clock.
registerAgent
  :: MonadIO m
  => Connection
  -> Alias
  -> Text -- ^ path
  -> Timestamp
  -> m (Timestamp, Timestamp) -- ^ (registered_at, updated_at)
registerAgent c alias path now = liftIO $ do
  existing <-
    query
      c
      "SELECT registered_at, updated_at FROM agents WHERE alias = ?"
      (Only alias)
      :: IO [(Timestamp, Timestamp)]
  case existing of
    (reg, _) : _ -> do
      execute
        c
        "UPDATE agents SET path = ?, updated_at = ? WHERE alias = ?"
        (path, now, alias)
      pure (reg, now)
    [] -> do
      execute
        c
        "INSERT INTO agents (alias, path, summary, tags, registered_at, updated_at)\n\
        \VALUES (?, ?, NULL, NULL, ?, ?)"
        (alias, path, now, now)
      pure (now, now)

-- | Replace summary/tags/endpoints for an existing agent atomically.
putProfile
  :: MonadIO m
  => Connection
  -> Alias
  -> ProfileInput
  -> Timestamp
  -> m (Int, Timestamp)
putProfile c alias ProfileInput {..} now = liftIO $ do
  let tagsJson = jsonToText (A.toJSON piTags)
  withTransaction c $ do
    execute
      c
      "UPDATE agents SET summary = ?, tags = ?, updated_at = ? WHERE alias = ?"
      (piSummary, tagsJson, now, alias)
    execute
      c
      "DELETE FROM endpoints WHERE agent_alias = ?"
      (Only alias)
    mapM_ (insertEndpoint c alias) piEndpoints
  pure (length piEndpoints, now)

insertEndpoint :: Connection -> Alias -> EndpointInput -> IO ()
insertEndpoint c alias EndpointInput {..} =
  execute
    c
    "INSERT INTO endpoints (agent_alias, verb, arg_schema, param_schema, autonomy, description)\n\
    \VALUES (?, ?, ?, ?, ?, ?)"
    ( alias
    , eiVerb
    , jsonToText <$> eiArgSchema
    , jsonToText <$> eiParamSchema
    , autonomyText eiAutonomy
    , eiDescription
    )
