module Poreus.Session
  ( -- * Row
    SessionRow (..)

    -- * Lifecycle
  , ensureSession
  , endSession

    -- * Queries
  , getSession
  , listSessions

    -- * Liveness
  , sessionLive
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Database.SQLite.Simple (Connection, Only (..), execute, query, query_)
import Database.SQLite.Simple.FromRow (FromRow (..), field)

import Poreus.Effects.SystemInfo (CanSystemInfo, getBootId, getProcessStartTime, isPidAlive)
import Poreus.Effects.Time (CanTime, currentTime)
import Poreus.Time (Timestamp (..))
import Poreus.Types (SessionAddress (..))

-- | One agent session (spec §5): the unit that sends, receives, and
-- attends. Born at first contact (REG-2), it owns exactly one mailbox
-- keyed by its address.
--
-- Note [The liveness triple]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- `pid`, `boot_id` and `proc_start` together identify the serving
-- process exactly. All three are needed:
--
--   * `pid` alone is reused. `pid_max` is 4194304 on a typical Linux
--     host, so a stale row can name a pid that some unrelated process
--     now holds — and the resulting lie is \"alive\", which is the
--     direction that misroutes messages.
--   * `boot_id` catches reboots but not reuse within one boot.
--   * `proc_start` (field 22 of /proc/<pid>/stat, in clock ticks since
--     boot) makes the identity exact: a reused pid has a later start
--     time than the one recorded.
--
-- They are NULL when the row was last touched by a process that does
-- not own the session — the hook companion never overwrites them.
--
-- `last_seen_at` is NOT a liveness signal. v0.3 stored a heartbeat and
-- used its freshness to decide attendance; the writing thread died and
-- the field lied for 45 h (ADR-0017, L1/L2). It now serves retention
-- only, and liveness reads the triple against the OS every time.
data SessionRow = SessionRow
  { sessAddress :: !SessionAddress
  , sessWorkspace :: !Text
  , sessPid :: !(Maybe Int)
  , sessBootId :: !(Maybe Text)
  , sessProcStart :: !(Maybe Integer)
  , sessFirstSeenAt :: !Timestamp
  , sessLastSeenAt :: !Timestamp
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
      <*> field

-- | Upsert the session at (first) contact: creates the row and its
-- cursor on first sight, refreshes `last_seen_at`/workspace
-- afterwards, and clears `ended_at` (a resumed session revives its
-- address, RECV-5). `pid`/`boot_id`/`proc_start` are only overwritten
-- when a pid is supplied — the hook passes Nothing so it never
-- masquerades as the serving process. The start time is read from the
-- OS here rather than passed in, so a caller cannot record a triple
-- that never existed.
ensureSession ::
  (CanTime m, CanSystemInfo m, MonadIO m) =>
  Connection ->
  SessionAddress ->
  Text ->
  Maybe Int ->
  Maybe Text ->
  m SessionRow
ensureSession c addr workspace mpid mboot = do
  now <- Timestamp <$> currentTime
  mstart <- maybe (pure Nothing) getProcessStartTime mpid
  liftIO $ do
    execute
      c
      "INSERT INTO sessions (address, workspace, pid, boot_id, proc_start, first_seen_at, last_seen_at, ended_at)\n\
      \VALUES (?, ?, ?, ?, ?, ?, ?, NULL)\n\
      \ON CONFLICT(address) DO UPDATE SET\n\
      \  workspace = excluded.workspace,\n\
      \  pid = COALESCE(excluded.pid, pid),\n\
      \  boot_id = COALESCE(excluded.boot_id, boot_id),\n\
      \  proc_start = COALESCE(excluded.proc_start, proc_start),\n\
      \  last_seen_at = excluded.last_seen_at,\n\
      \  ended_at = NULL"
      (addr, workspace, mpid, mboot, mstart, now, now)
    execute
      c
      "INSERT OR IGNORE INTO cursors (session_address, last_seq) VALUES (?, 0)"
      (Only addr)
  rows <- getSession c addr
  case rows of
    Just r -> pure r
    -- Unreachable: the row was just upserted.
    Nothing -> error "ensureSession: row vanished"

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
      "SELECT address, workspace, pid, boot_id, proc_start, first_seen_at, last_seen_at, ended_at\n\
      \FROM sessions WHERE address = ?"
      (Only addr)
  pure $ case rows of
    (r : _) -> Just r
    [] -> Nothing

listSessions :: MonadIO m => Connection -> m [SessionRow]
listSessions c = liftIO $ do
  query_
    c
    "SELECT address, workspace, pid, boot_id, proc_start, first_seen_at, last_seen_at, ended_at\n\
    \FROM sessions ORDER BY first_seen_at, address"

-- | Is this session's serving process still running? Dead when
-- explicitly ended; otherwise the recorded triple is compared against
-- the OS, right now (ADR-0017).
--
-- This deliberately answers a narrower question than v0.3's
-- `sessionLive` did. It reports that a process EXISTS, not that the
-- session is attending: a wedged `claude` still reads alive. Waking an
-- idle session is the host's job now, and poreus only promises to
-- queue, so the narrower fact is the one it can actually own.
--
-- A row with no recorded pid reads as live, which looks like the
-- false-alive this design set out to delete but is not. `pid` is only
-- ever NULL when no serving process has yet identified itself — the
-- hook companion passes Nothing so it cannot masquerade as the server,
-- and `ensureSession` COALESCEs, so a pid once recorded is never
-- erased. So NULL means "a live session the server has not answered
-- for yet", typically between `SessionStart` and the first tool call,
-- and calling that dead would break the hook's own auto-claim.
--
-- The residual gap: a hook-created row for a session killed without
-- `endSession` keeps reading live. `doctor` flags it by comparing
-- against the host session file, which is where that fact actually
-- lives.
sessionLive :: CanSystemInfo m => SessionRow -> m Bool
sessionLive row = case sessEndedAt row of
  Just _ -> pure False
  Nothing -> case (sessPid row, sessBootId row) of
    (Just pid, Just boot) -> do
      curBoot <- getBootId
      if boot /= curBoot
        then pure False
        else do
          alive <- isPidAlive pid
          if not alive
            then pure False
            else case sessProcStart row of
              -- Row predates the triple: pid+boot is all there is.
              Nothing -> pure True
              Just recorded -> do
                actual <- getProcessStartTime pid
                pure (actual == Just recorded)
    _ -> pure True
