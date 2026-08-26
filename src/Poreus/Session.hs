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

    -- * The host's own view of a session
  , liveHostPidOf
  , liveHostNameOf
  , liveHostNameOfRow
  , hostNamesByAddress
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection, Only (..), execute, query, query_)
import Database.SQLite.Simple.FromRow (FromRow (..), field)

import Poreus.Effects.Env (CanEnv)
import Poreus.Effects.FileSystem (CanFileSystem)
import Poreus.Effects.SystemInfo (CanSystemInfo, getBootId, getProcessStartTime, isPidAlive)
import Poreus.Effects.Time (CanTime, currentTime)
import Poreus.HostSession (HostSession (..), readHostSession)
import Poreus.Identity (claudeAncestorOf)
import Poreus.Time (Timestamp (..))
import Poreus.Types (SessionAddress (..), sessionAddressPrefix)

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
-- Note [The host's name is not stored]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- v0.4 briefly kept a `host_name` column: the host's own name for the
-- session, renewed on every contact. It was deleted for the same
-- reason as the heartbeat, one level up. A stored copy is refreshed
-- when the session is ACTIVE, and every consumer of it — the doorbell,
-- the catalog's advertised ring target, a refusal telling somebody
-- which window to look at — exists to describe a session that is
-- IDLE. The renewal was anti-correlated with the need, so the value
-- was least trustworthy exactly where it was used.
--
-- Read it from the host's session file instead, at the moment it is
-- needed: 'liveHostNameOf' for one session, 'hostNamesByAddress' for a
-- listing.
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
      "INSERT OR IGNORE INTO cursors (mailbox, last_seq) VALUES (?, 0)"
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

-- | The claude process a session belongs to, as of now.
--
-- Note [Two pid namespaces]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- `sessions.pid` is the pid of the `poreus serve` process. The host
-- keys its session files by the pid of the *claude* process that
-- spawned it. Those are different numbers — measured on this host,
-- 3767388 and 3767222 for one session — and comparing one against the
-- other is always a mismatch. The first shipped `doctor` did exactly
-- that, so both of its host comparisons were false on every real
-- session.
--
-- The two `proc_start` columns are likewise two different processes'
-- start times, one tick-second apart for a parent/child pair, and are
-- never compared with each other.
--
-- Note [The claude pid comes from the process tree]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The two pid namespaces are not two unrelated facts: the serve
-- process is a CHILD of the claude process, so one hop up the parent
-- chain crosses between them. That hop is an operating-system fact,
-- and `sessions.pid` is already checked against the OS on every read
-- ('sessionLive'). The whole join is therefore derivable at the moment
-- it is needed, from state nobody has to keep fresh.
--
-- The first version instead read `host_sessions` and did
-- @lookup addr@ over an unordered @SELECT@. Two defects, one
-- structure:
--
--   * The map is keyed by process instance, not by session (ADR-0016
--     §2), so one session id legitimately carries SEVERAL rows — one
--     per claude process that ever presented it. `claude --resume` in
--     a fresh window adds one; so does a reboot. `lookup` is a
--     single-valued read of a many-valued map, and with no @ORDER BY@
--     it took whatever SQLite scanned first — rowid order, so the
--     OLDEST row won. Measured 2026-08-26: 78 rows, 69 of them naming
--     a dead pid, 6 session ids carrying more than one row, and in all
--     6 the first row was the dead one. `poreus doctor` called 8 live,
--     healthy, named sessions broken, and the doorbell was withheld
--     from every one of them.
--   * The map is written when a session CONTACTS poreus — that is,
--     when it is ACTIVE. The doorbell exists to reach a session that
--     is IDLE. This is the anti-correlation that deleted
--     `sessions.host_name` (see Note [The host's name is not stored]),
--     one level down: that fix demoted the name and left the pid cache
--     under it still routing. Measured 2026-08-26: a window started at
--     09:24:43 was unringable until 09:31:08, because the row naming
--     its pid did not exist yet.
--
-- The map survives only as a fallback, for a row no serving process
-- has ever written a pid into — a hook-only session, where there is no
-- child to walk up from. That path is now boot-scoped, ordered
-- newest-first, and filtered to a pid the OS confirms.
--
-- Not attempted: matching a session to a window by workspace. Two live
-- sessions shared one repository on 2026-08-18 and a workspace match
-- rang the wrong one.
liveHostPidOf ::
  (CanSystemInfo m, MonadIO m) =>
  Connection ->
  SessionRow ->
  m (Maybe Int)
liveHostPidOf c row = do
  fromTree <- case sessPid row of
    Nothing -> pure Nothing
    Just servePid -> claudeAncestorOf servePid
  case fromTree of
    Just pid -> pure (Just pid)
    Nothing -> do
      boot <- getBootId
      hostPidsFor c (sessionIdOf (sessAddress row)) boot >>= firstAlive
  where
    firstAlive [] = pure Nothing
    firstAlive (p : ps) = do
      alive <- isPidAlive p
      if alive then pure (Just p) else firstAlive ps

-- | Claude pids the identity map has recorded for a session id, this
-- boot only, most recently seen first. Several is normal; see
-- Note [The claude pid comes from the process tree].
hostPidsFor :: MonadIO m => Connection -> Text -> Text -> m [Int]
hostPidsFor c sid boot =
  liftIO $
    map fromOnly
      <$> query
        c
        "SELECT host_pid FROM host_sessions\n\
        \WHERE session_id = ? AND boot_id = ?\n\
        \ORDER BY updated_at DESC"
        (sid, boot)

-- | The bare session id inside an address. The address is the id
-- behind a fixed prefix; `host_sessions` keys on the id.
sessionIdOf :: SessionAddress -> Text
sessionIdOf (SessionAddress a) =
  fromMaybe a (T.stripPrefix sessionAddressPrefix a)

-- | The host's name for a session *right now*, read from its session
-- file rather than from anything poreus stored.
--
-- Use this wherever a name is shown to somebody who will act on it. A
-- stale name is worse than none: on 2026-08-19 a `name-held` refusal
-- printed a stored `nixos-65` for a session the user had renamed to
-- `kairos-hermes`, and a peer searched the host's live-session list
-- for `nixos-65`, found nothing, and concluded the holder was dead. It
-- was not; it was one row above where it looked.
liveHostNameOf ::
  (CanEnv m, CanFileSystem m, CanSystemInfo m, MonadIO m) =>
  Connection ->
  SessionAddress ->
  m (Maybe Text)
liveHostNameOf c addr = do
  mrow <- getSession c addr
  maybe (pure Nothing) (liveHostNameOfRow c) mrow

-- | 'liveHostNameOf' for a caller that already holds the row, which
-- saves the read it would otherwise repeat.
liveHostNameOfRow ::
  (CanEnv m, CanFileSystem m, CanSystemInfo m, MonadIO m) =>
  Connection ->
  SessionRow ->
  m (Maybe Text)
liveHostNameOfRow c row = do
  mpid <- liveHostPidOf c row
  case mpid of
    Nothing -> pure Nothing
    Just pid -> do
      mhs <- readHostSession pid
      pure (mhs >>= hsName)

-- | Every live session the host currently names.
--
-- One resolution per session, the same one 'liveHostNameOf' does. The
-- earlier version read the map once and every session file once, which
-- was cheaper and wrong for the reason in
-- Note [The claude pid comes from the process tree]. N is the number of
-- live sessions on one host, so the cost is a few /proc reads.
hostNamesByAddress ::
  (CanEnv m, CanFileSystem m, CanSystemInfo m, MonadIO m) =>
  Connection ->
  m [(SessionAddress, Text)]
hostNamesByAddress c = do
  rows <- filter (isNothing . sessEndedAt) <$> listSessions c
  catMaybes <$> mapM one rows
  where
    one row = do
      mname <- liveHostNameOfRow c row
      pure ((sessAddress row,) <$> mname)
