module Poreus.Types
  ( -- * Identifiers
    Alias (..)
  , TaskId (..)
    -- * Enumerations
  , Autonomy (..)
  , autonomyText
  , parseAutonomy
    -- * Rows
  , Agent (..)
  , Endpoint (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withText, (.=))
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField (ToField)
import GHC.Generics (Generic)

import Poreus.Time (Timestamp)

newtype Alias = Alias {unAlias :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (IsString, ToJSON, FromJSON, ToField, FromField)

-- | Message identifier: "YYYYMMDD-HHmmss-<from>-<4hex>". Stored as TEXT.
-- Named `TaskId` for historical reasons; semantically it's the identifier
-- of a row in the `messages` table.
newtype TaskId = TaskId {unTaskId :: Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (IsString, ToJSON, FromJSON, ToField, FromField)

data Autonomy = AutonomyAuto | AutonomyConfirm
  deriving stock (Show, Eq, Generic)

autonomyText :: Autonomy -> Text
autonomyText = \case
  AutonomyAuto -> "auto"
  AutonomyConfirm -> "confirm"

parseAutonomy :: Text -> Maybe Autonomy
parseAutonomy = \case
  "auto" -> Just AutonomyAuto
  "confirm" -> Just AutonomyConfirm
  _ -> Nothing

instance ToJSON Autonomy where
  toJSON = toJSON . autonomyText

instance FromJSON Autonomy where
  parseJSON = withText "Autonomy" $ \t ->
    case parseAutonomy t of
      Just v -> pure v
      Nothing -> fail ("invalid autonomy: " <> T.unpack t)

data Agent = Agent
  { agentAlias :: !Alias
  , agentPath :: !Text
  , agentSummary :: !(Maybe Text)
  , agentTags :: ![Text]
  , agentRegisteredAt :: !Timestamp
  , agentUpdatedAt :: !Timestamp
  , agentEndpoints :: ![Endpoint]
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON Agent where
  toJSON a =
    object
      [ "alias" .= agentAlias a
      , "path" .= agentPath a
      , "summary" .= agentSummary a
      , "tags" .= agentTags a
      , "registered_at" .= agentRegisteredAt a
      , "updated_at" .= agentUpdatedAt a
      , "endpoints" .= agentEndpoints a
      ]

data Endpoint = Endpoint
  { endpointAgent :: !Alias
  , endpointVerb :: !Text
  , endpointArgSchema :: !(Maybe Value)
  , endpointParamSchema :: !(Maybe Value)
  , endpointAutonomy :: !Autonomy
  , endpointDescription :: !(Maybe Text)
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON Endpoint where
  toJSON e =
    object
      [ "agent" .= endpointAgent e
      , "verb" .= endpointVerb e
      , "arg_schema" .= endpointArgSchema e
      , "param_schema" .= endpointParamSchema e
      , "autonomy" .= endpointAutonomy e
      , "description" .= endpointDescription e
      ]
