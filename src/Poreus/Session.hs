module Poreus.Session
  ( -- * Row
    SessionRow (..)

    -- * Lifecycle
  , ensureSession
  , heartbeat
  , endSession

    -- * Queries
  , getSession
  , listSessions

    -- * Liveness
  , sessionLive
  , livenessWindowSeconds
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Time (diffUTCTime)
import Database.SQLite.Simple (Connection, Only (..), execute, query, query_)
import Database.SQLite.Simple.FromRow (FromRow (..), field)

import Poreus.Effects.SystemInfo (CanSystemInfo, getBootId, isPidAlive)
import Poreus.Effects.Time (CanTime, currentTime)
import Poreus.Time (Timestamp (..), unTimestamp)
import Poreus.Types (SessionAddress (..))

-- | One agent session (spec §5): the unit that sends, receives, and
-- attends. Born at first contact (REG-2), it owns exactly one mailbox
-- keyed by its address. `pid`/`boot_id` identify the serving process
-- for liveness corroboration; they are NULL when the row was last
-- touched by a process that does not own the session (the hook
-- companion never overwrites them).
data SessionRow = SessionRow
  { sessAddress :: !SessionAddress
  , sessWorkspace :: !Text
  , sessPid :: !(Maybe Int)
  , sessBootId :: !(Maybe Text)
  , sessFirstSeenAt :: !Timestamp
  , sessHeartbeatAt :: !Timestamp
  , sessEndedAt :: !(Maybe Timestamp)
  }
  deriving stock (Show, Eq)

instance FromRow SessionRow where
  fromRow =
    SessionRow
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

-- | Upsert the session at (first) contact: creates the row and its
-- cursor on first sight, refreshes heartbeat/workspace afterwards, and
-- clears `ended_at` (a resumed session revives its address, RECV-5).
-- `pid`/`boot_id` are only overwritten when supplied — the hook passes
-- Nothing so it never masquerades as the serving process.
ensureSession ::
  (CanTime m, MonadIO m) =>
  Connection ->
  SessionAddress ->
  Text ->
  Maybe Int ->
  Maybe Text ->
  m SessionRow
ensureSession c addr workspace mpid mboot = do
  now <- Timestamp <$> currentTime
  liftIO $ do
    execute
      c
      "INSERT INTO sessions (address, workspace, pid, boot_id, first_seen_at, heartbeat_at, ended_at)\n\
      \VALUES (?, ?, ?, ?, ?, ?, NULL)\n\
      \ON CONFLICT(address) DO UPDATE SET\n\
      \  workspace = excluded.workspace,\n\
      \  pid = COALESCE(excluded.pid, pid),\n\
      \  boot_id = COALESCE(excluded.boot_id, boot_id),\n\
      \  heartbeat_at = excluded.heartbeat_at,\n\
      \  ended_at = NULL"
      (addr, workspace, mpid, mboot, now, now)
    execute
      c
      "INSERT OR IGNORE INTO cursors (session_address, last_seq) VALUES (?, 0)"
      (Only addr)
  rows <- getSession c addr
  case rows of
    Just r -> pure r
    -- Unreachable: the row was just upserted.
    Nothing -> error "ensureSession: row vanished"

heartbeat :: (CanTime m, MonadIO m) => Connection -> SessionAddress -> m ()
heartbeat c addr = do
  now <- Timestamp <$> currentTime
  liftIO $ execute c "UPDATE sessions SET heartbeat_at = ? WHERE address = ?" (now, addr)

-- | Mark the session ended and release any name it holds (REG-3: a
-- released name and its profile stay intact for the next claimant).
endSession :: (CanTime m, MonadIO m) => Connection -> SessionAddress -> m ()
endSession c addr = do
  now <- Timestamp <$> currentTime
  liftIO $ do
    execute c "UPDATE sessions SET ended_at = ? WHERE address = ?" (now, addr)
    execute
      c
      "UPDATE names SET bound_session = NULL, bound_at = NULL WHERE bound_session = ?"
      (Only addr)

getSession :: MonadIO m => Connection -> SessionAddress -> m (Maybe SessionRow)
getSession c addr = liftIO $ do
  rows <-
    query
      c
      "SELECT address, workspace, pid, boot_id, first_seen_at, heartbeat_at, ended_at\n\
      \FROM sessions WHERE address = ?"
      (Only addr)
  pure $ case rows of
    (r : _) -> Just r
    [] -> Nothing

listSessions :: MonadIO m => Connection -> m [SessionRow]
listSessions c = liftIO $ do
  query_
    c
    "SELECT address, workspace, pid, boot_id, first_seen_at, heartbeat_at, ended_at\n\
    \FROM sessions ORDER BY first_seen_at, address"

-- | Attendance heartbeat freshness window. The serving process ticks
-- every ~5 s; three missed ticks means it is gone.
livenessWindowSeconds :: Double
livenessWindowSeconds = 15

-- | Is this session alive (and hence attending, RECV-1)? Dead when
-- explicitly ended; otherwise pid+boot_id corroboration is the fast
-- path to declare death (boot changed, or serving process gone), and
-- heartbeat freshness decides the rest.
sessionLive :: (CanTime m, CanSystemInfo m) => SessionRow -> m Bool
sessionLive row = case sessEndedAt row of
  Just _ -> pure False
  Nothing -> do
    corroboration <- case (sessPid row, sessBootId row) of
      (Just pid, Just boot) -> do
        curBoot <- getBootId
        if boot /= curBoot
          then pure False
          else isPidAlive pid
      _ -> pure True
    if not corroboration
      then pure False
      else do
        now <- currentTime
        let age = diffUTCTime now (unTimestamp (sessHeartbeatAt row))
        pure (age >= 0 && realToFrac age <= livenessWindowSeconds)
