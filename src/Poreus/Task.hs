{-# OPTIONS_GHC -Wno-orphans #-}

module Poreus.Task
  ( SendInput (..)
  , CompleteInput (..)
  , loadTask
  , loadResult
  , newTaskId
  , sendTask
  , inboxTasks
  , sentTasks
  , allTasksForAlias
  , claimTask
  , completeTask
  , rejectTask
  , lookupAgentPath
  , TaskErr (..)
  , fromTransitionErr
  , taskFields
  ) where

import qualified Data.Aeson as A
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection, Only (..), Query (..), execute, query, withTransaction)
import Database.SQLite.Simple.FromRow (FromRow (..), field)

import qualified Poreus.Message as Msg
import Poreus.Profile (jsonToText, textToJson)
import Poreus.Time (Timestamp, formatTaskStamp, unTimestamp)
import Poreus.Types

data SendInput = SendInput
  { siTo :: !Alias
  , siKind :: !TaskKind
  , siUrl :: !(Maybe Text)
  , siDescription :: !(Maybe Text)
  , siExpected :: !(Maybe Text)
  }
  deriving stock (Show, Eq)

instance A.FromJSON SendInput where
  parseJSON = A.withObject "SendInput" $ \o ->
    SendInput
      <$> o A..: "to"
      <*> o A..: "kind"
      <*> o A..:? "url"
      <*> o A..:? "description"
      <*> o A..:? "expected"

data CompleteInput = CompleteInput
  { ciStatus :: !ResultStatus
  , ciSummary :: !(Maybe Text)
  , ciArtifacts :: !(Maybe A.Value)
  }
  deriving stock (Show, Eq)

instance A.FromJSON CompleteInput where
  parseJSON = A.withObject "CompleteInput" $ \o ->
    CompleteInput
      <$> o A..: "status"
      <*> o A..:? "summary"
      <*> o A..:? "artifacts"

instance FromRow Task where
  fromRow = do
    tid <- field
    from <- field
    to <- field
    kind <- field
    url <- field
    desc <- field
    expected <- field
    stat <- field
    createdAt <- field
    claimedAt <- field
    completedAt <- field
    pure
      Task
        { taskId = TaskId tid
        , taskFrom = Alias from
        , taskTo = Alias to
        , taskKind = maybe KindFreetext id (parseTaskKind kind)
        , taskUrl = url
        , taskDescription = desc
        , taskExpected = expected
        , taskStatus = maybe TSPending id (parseTaskStatus stat)
        , taskCreatedAt = createdAt
        , taskClaimedAt = claimedAt
        , taskCompletedAt = completedAt
        }

taskFields :: T.Text
taskFields =
  T.concat
    [ "id, from_alias, to_alias, kind, url, description, expected,"
    , " status, created_at, claimed_at, completed_at"
    ]

-- | Pure formatter: "YYYYMMDD-HHmmss-<from>-<4hex>". Caller supplies the
-- wall-clock stamp and random hex tail.
newTaskId :: Alias -> Timestamp -> Text -> TaskId
newTaskId (Alias from) ts hex =
  TaskId (T.concat [formatTaskStamp (unTimestamp ts), "-", from, "-", hex])

data TaskErr = TaskNotFound !Text | TaskBadTransition !Text
  deriving stock (Show, Eq)

fromTransitionErr :: TransitionErr -> TaskErr
fromTransitionErr (TransitionForbidden s t) =
  TaskBadTransition
    ( "cannot "
        <> transitionVerb t
        <> " task in status "
        <> taskStatusText s
    )
  where
    transitionVerb TrClaim = "claim"
    transitionVerb TrComplete = "complete"
    transitionVerb TrFail = "fail"
    transitionVerb TrReject = "reject"

-- | Insert a `pending` task AND its corresponding `messages` row
-- (kind=request). Both rows share the same id.
sendTask
  :: MonadIO m
  => Connection
  -> Alias -- ^ from
  -> TaskId
  -> Timestamp -- ^ now
  -> SendInput
  -> m Task
sendTask c from tid now SendInput {..} = liftIO $ withTransaction c $ do
  execute
    c
    "INSERT INTO tasks (id, from_alias, to_alias, kind, url, description, expected,\n\
    \                   status, created_at, claimed_at, completed_at)\n\
    \VALUES (?, ?, ?, ?, ?, ?, ?, 'pending', ?, NULL, NULL)"
    ( tid
    , from
    , siTo
    , taskKindText siKind
    , siUrl
    , siDescription
    , siExpected
    , now
    )
  Msg.insertMessage
    c
    Msg.Message
      { Msg.msgId = tid
      , Msg.msgFrom = from
      , Msg.msgTo = siTo
      , Msg.msgKind = Msg.MKRequest
      , Msg.msgInReplyTo = Nothing
      , Msg.msgPayload =
          A.toJSON
            ( Msg.RequestPayload
                { Msg.rpRequestKind = siKind
                , Msg.rpUrl = siUrl
                , Msg.rpDescription = siDescription
                , Msg.rpExpected = siExpected
                }
            )
      , Msg.msgCreatedAt = now
      }
  pure
    Task
      { taskId = tid
      , taskFrom = from
      , taskTo = siTo
      , taskKind = siKind
      , taskUrl = siUrl
      , taskDescription = siDescription
      , taskExpected = siExpected
      , taskStatus = TSPending
      , taskCreatedAt = now
      , taskClaimedAt = Nothing
      , taskCompletedAt = Nothing
      }

loadTask :: MonadIO m => Connection -> TaskId -> m (Maybe Task)
loadTask c tid = liftIO $ do
  rows <-
    query
      c
      (fromText ("SELECT " <> taskFields <> " FROM tasks WHERE id = ?"))
      (Only tid)
  case rows of
    [] -> pure Nothing
    (t : _) -> pure (Just t)

inboxTasks :: MonadIO m => Connection -> Alias -> Maybe TaskStatus -> m [Task]
inboxTasks c alias mstatus = liftIO $ case mstatus of
  Just s ->
    query
      c
      (fromText ("SELECT " <> taskFields <> " FROM tasks WHERE to_alias = ? AND status = ? ORDER BY created_at"))
      (alias, taskStatusText s)
  Nothing ->
    query
      c
      (fromText ("SELECT " <> taskFields <> " FROM tasks WHERE to_alias = ? ORDER BY created_at"))
      (Only alias)

sentTasks :: MonadIO m => Connection -> Alias -> m [Task]
sentTasks c alias = liftIO $
  query
    c
    (fromText ("SELECT " <> taskFields <> " FROM tasks WHERE from_alias = ? ORDER BY created_at"))
    (Only alias)

allTasksForAlias :: MonadIO m => Connection -> Alias -> m [Task]
allTasksForAlias c alias = liftIO $
  query
    c
    (fromText ("SELECT " <> taskFields <> " FROM tasks WHERE from_alias = ? OR to_alias = ? ORDER BY created_at"))
    (alias, alias)

claimTask
  :: MonadIO m
  => Connection
  -> TaskId
  -> Timestamp
  -> m (Either TaskErr Task)
claimTask c tid now = do
  mt <- loadTask c tid
  case mt of
    Nothing -> pure (Left (TaskNotFound ("task not found: " <> unTaskId tid)))
    Just t -> case validateTransition (taskStatus t) TrClaim of
      Left err -> pure (Left (fromTransitionErr err))
      Right next -> liftIO $ do
        execute
          c
          "UPDATE tasks SET status = ?, claimed_at = ? WHERE id = ?"
          (taskStatusText next, now, tid)
        pure
          ( Right
              t
                { taskStatus = next
                , taskClaimedAt = Just now
                }
          )

-- | Complete (or fail) a previously-claimed task. In addition to the
-- legacy `tasks` status update and `results` row, a new `messages` row
-- is written with `kind=reply`, `in_reply_to = taskId`. Caller supplies
-- the reply message id (built from their own now+hex via `newTaskId`).
completeTask
  :: MonadIO m
  => Connection
  -> TaskId -- ^ original task id
  -> TaskId -- ^ new reply-message id
  -> Timestamp
  -> CompleteInput
  -> m (Either TaskErr (Task, TaskResult))
completeTask c tid replyId now CompleteInput {..} = do
  mt <- loadTask c tid
  case mt of
    Nothing -> pure (Left (TaskNotFound ("task not found: " <> unTaskId tid)))
    Just t -> case pickTransition ciStatus of
      Left msg -> pure (Left (TaskBadTransition msg))
      Right (transition, targetTaskStatus, targetResultStatus) ->
        case validateTransition (taskStatus t) transition of
          Left err -> pure (Left (fromTransitionErr err))
          Right _ -> liftIO (finish t targetTaskStatus targetResultStatus)
  where
    pickTransition :: ResultStatus -> Either Text (Transition, TaskStatus, ResultStatus)
    pickTransition RSCompleted = Right (TrComplete, TSCompleted, RSCompleted)
    pickTransition RSFailed = Right (TrFail, TSFailed, RSFailed)
    pickTransition RSRejected = Left "use `poreus reject` for rejection"

    finish t tstat rstat = do
      let arts = maybe (A.Array mempty) id ciArtifacts
      withTransaction c $ do
        execute
          c
          "UPDATE tasks SET status = ?, completed_at = ? WHERE id = ?"
          (taskStatusText tstat, now, tid)
        execute
          c
          "INSERT OR REPLACE INTO results (task_id, status, summary, artifacts, completed_at)\n\
          \VALUES (?, ?, ?, ?, ?)"
          (tid, resultStatusText rstat, ciSummary, jsonToText arts, now)
        Msg.insertMessage
          c
          Msg.Message
            { Msg.msgId = replyId
            , Msg.msgFrom = taskTo t -- the agent that completed
            , Msg.msgTo = taskFrom t
            , Msg.msgKind = Msg.MKReply
            , Msg.msgInReplyTo = Just tid
            , Msg.msgPayload =
                A.toJSON
                  ( Msg.ReplyPayload
                      { Msg.rplStatus = rstat
                      , Msg.rplSummary = ciSummary
                      , Msg.rplArtifacts = arts
                      , Msg.rplReason = Nothing
                      }
                  )
            , Msg.msgCreatedAt = now
            }
      let tr =
            TaskResult
              { resultTaskId = tid
              , resultStatus = rstat
              , resultSummary = ciSummary
              , resultArtifacts = arts
              , resultCompletedAt = now
              }
          t' = t {taskStatus = tstat, taskCompletedAt = Just now}
      pure (Right (t', tr))

-- | Reject a pending or claimed task. Writes a reply message with
-- `status=rejected` and the user's `reason` in both `summary` (for
-- backward compatibility with the legacy results view) and the
-- `reason` field of the payload.
rejectTask
  :: MonadIO m
  => Connection
  -> TaskId
  -> TaskId -- ^ new reply-message id
  -> Timestamp
  -> Text -- ^ reason
  -> m (Either TaskErr (Task, TaskResult))
rejectTask c tid replyId now reason = do
  mt <- loadTask c tid
  case mt of
    Nothing -> pure (Left (TaskNotFound ("task not found: " <> unTaskId tid)))
    Just t -> case validateTransition (taskStatus t) TrReject of
      Left err -> pure (Left (fromTransitionErr err))
      Right _ -> liftIO (doReject t)
  where
    doReject t = do
      let arts = A.Array mempty
      withTransaction c $ do
        execute
          c
          "UPDATE tasks SET status = 'rejected', completed_at = ? WHERE id = ?"
          (now, tid)
        execute
          c
          "INSERT OR REPLACE INTO results (task_id, status, summary, artifacts, completed_at)\n\
          \VALUES (?, 'rejected', ?, '[]', ?)"
          (tid, reason, now)
        Msg.insertMessage
          c
          Msg.Message
            { Msg.msgId = replyId
            , Msg.msgFrom = taskTo t
            , Msg.msgTo = taskFrom t
            , Msg.msgKind = Msg.MKReply
            , Msg.msgInReplyTo = Just tid
            , Msg.msgPayload =
                A.toJSON
                  ( Msg.ReplyPayload
                      { Msg.rplStatus = RSRejected
                      , Msg.rplSummary = Just reason
                      , Msg.rplArtifacts = arts
                      , Msg.rplReason = Just reason
                      }
                  )
            , Msg.msgCreatedAt = now
            }
      let tr =
            TaskResult
              { resultTaskId = tid
              , resultStatus = RSRejected
              , resultSummary = Just reason
              , resultArtifacts = arts
              , resultCompletedAt = now
              }
          t' = t {taskStatus = TSRejected, taskCompletedAt = Just now}
      pure (Right (t', tr))

loadResult :: MonadIO m => Connection -> TaskId -> m (Maybe TaskResult)
loadResult c tid = liftIO $ do
  rows <-
    query
      c
      "SELECT status, summary, artifacts, completed_at FROM results WHERE task_id = ?"
      (Only tid)
      :: IO [(Text, Maybe Text, Maybe Text, Timestamp)]
  case rows of
    [] -> pure Nothing
    (stat, summ, arts, doneAt) : _ ->
      pure $
        Just
          TaskResult
            { resultTaskId = tid
            , resultStatus = maybe RSCompleted id (parseResultStatus stat)
            , resultSummary = summ
            , resultArtifacts = maybe (A.Array mempty) id (arts >>= textToJson)
            , resultCompletedAt = doneAt
            }

lookupAgentPath :: MonadIO m => Connection -> Alias -> m (Maybe Text)
lookupAgentPath c alias = liftIO $ do
  rows <-
    query
      c
      "SELECT path FROM agents WHERE alias = ?"
      (Only alias)
      :: IO [Only Text]
  case rows of
    [] -> pure Nothing
    Only p : _ -> pure (Just p)

fromText :: T.Text -> Query
fromText = Query
