module Poreus.Types
  ( -- * Identifiers
    Alias (..)
  , TaskId (..)
    -- * Enumerations
  , Autonomy (..)
  , autonomyText
  , parseAutonomy
  , TaskKind (..)
  , taskKindText
  , parseTaskKind
  , TaskStatus (..)
  , taskStatusText
  , parseTaskStatus
  , ResultStatus (..)
  , resultStatusText
  , parseResultStatus
    -- * Rows
  , Agent (..)
  , Endpoint (..)
  , Task (..)
  , TaskResult (..)
    -- * State-machine helpers
  , Transition (..)
  , TransitionErr (..)
  , validateTransition
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withText, (.=))
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

-- | Task identifier: "YYYYMMDD-HHmmss-<from>-<4hex>". Stored as TEXT; the
-- format is defined in "Poreus.Task" and is not decomposed elsewhere.
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
  toJSON = String . autonomyText

instance FromJSON Autonomy where
  parseJSON = withText "Autonomy" $ \t ->
    case parseAutonomy t of
      Just v -> pure v
      Nothing -> fail ("invalid autonomy: " <> T.unpack t)

data TaskKind = KindFreetext | KindRpc
  deriving stock (Show, Eq, Generic)

taskKindText :: TaskKind -> Text
taskKindText = \case
  KindFreetext -> "freetext"
  KindRpc -> "rpc"

parseTaskKind :: Text -> Maybe TaskKind
parseTaskKind = \case
  "freetext" -> Just KindFreetext
  "rpc" -> Just KindRpc
  _ -> Nothing

instance ToJSON TaskKind where
  toJSON = String . taskKindText

instance FromJSON TaskKind where
  parseJSON = withText "TaskKind" $ \t ->
    case parseTaskKind t of
      Just v -> pure v
      Nothing -> fail ("invalid kind: " <> T.unpack t)

data TaskStatus
  = TSPending
  | TSClaimed
  | TSCompleted
  | TSFailed
  | TSRejected
  deriving stock (Show, Eq, Generic)

taskStatusText :: TaskStatus -> Text
taskStatusText = \case
  TSPending -> "pending"
  TSClaimed -> "claimed"
  TSCompleted -> "completed"
  TSFailed -> "failed"
  TSRejected -> "rejected"

parseTaskStatus :: Text -> Maybe TaskStatus
parseTaskStatus = \case
  "pending" -> Just TSPending
  "claimed" -> Just TSClaimed
  "completed" -> Just TSCompleted
  "failed" -> Just TSFailed
  "rejected" -> Just TSRejected
  _ -> Nothing

instance ToJSON TaskStatus where
  toJSON = String . taskStatusText

instance FromJSON TaskStatus where
  parseJSON = withText "TaskStatus" $ \t ->
    case parseTaskStatus t of
      Just v -> pure v
      Nothing -> fail ("invalid task status: " <> T.unpack t)

data ResultStatus = RSCompleted | RSFailed | RSRejected
  deriving stock (Show, Eq, Generic)

resultStatusText :: ResultStatus -> Text
resultStatusText = \case
  RSCompleted -> "completed"
  RSFailed -> "failed"
  RSRejected -> "rejected"

parseResultStatus :: Text -> Maybe ResultStatus
parseResultStatus = \case
  "completed" -> Just RSCompleted
  "failed" -> Just RSFailed
  "rejected" -> Just RSRejected
  _ -> Nothing

instance ToJSON ResultStatus where
  toJSON = String . resultStatusText

instance FromJSON ResultStatus where
  parseJSON = withText "ResultStatus" $ \t ->
    case parseResultStatus t of
      Just v -> pure v
      Nothing -> fail ("invalid result status: " <> T.unpack t)

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

data Task = Task
  { taskId :: !TaskId
  , taskFrom :: !Alias
  , taskTo :: !Alias
  , taskKind :: !TaskKind
  , taskUrl :: !(Maybe Text)
  , taskDescription :: !(Maybe Text)
  , taskExpected :: !(Maybe Text)
  , taskStatus :: !TaskStatus
  , taskCreatedAt :: !Timestamp
  , taskClaimedAt :: !(Maybe Timestamp)
  , taskCompletedAt :: !(Maybe Timestamp)
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON Task where
  toJSON t =
    object
      [ "id" .= taskId t
      , "from" .= taskFrom t
      , "to" .= taskTo t
      , "kind" .= taskKind t
      , "url" .= taskUrl t
      , "description" .= taskDescription t
      , "expected" .= taskExpected t
      , "status" .= taskStatus t
      , "created_at" .= taskCreatedAt t
      , "claimed_at" .= taskClaimedAt t
      , "completed_at" .= taskCompletedAt t
      ]

data TaskResult = TaskResult
  { resultTaskId :: !TaskId
  , resultStatus :: !ResultStatus
  , resultSummary :: !(Maybe Text)
  , resultArtifacts :: !Value
  , resultCompletedAt :: !Timestamp
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON TaskResult where
  toJSON r =
    object
      [ "task_id" .= resultTaskId r
      , "status" .= resultStatus r
      , "summary" .= resultSummary r
      , "artifacts" .= resultArtifacts r
      , "completed_at" .= resultCompletedAt r
      ]

-- | Desired next state for `validateTransition`. Mirrors the set of
-- transitions the CLI supports.
data Transition
  = TrClaim
  | TrComplete
  | TrFail
  | TrReject
  deriving stock (Show, Eq)

data TransitionErr = TransitionForbidden !TaskStatus !Transition
  deriving stock (Show, Eq)

-- | Pure state-machine rule. Returns the resulting status or a structured
-- error. No IO, no DB — this is the single source of truth for what
-- transitions are legal and is tested directly.
validateTransition :: TaskStatus -> Transition -> Either TransitionErr TaskStatus
validateTransition TSPending TrClaim = Right TSClaimed
validateTransition TSPending TrReject = Right TSRejected
validateTransition TSClaimed TrComplete = Right TSCompleted
validateTransition TSClaimed TrFail = Right TSFailed
validateTransition TSClaimed TrReject = Right TSRejected
validateTransition s t = Left (TransitionForbidden s t)
