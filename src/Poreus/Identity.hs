module Poreus.Identity
  ( -- * Session identity (REG-2)
    Identity (..)
  , IdentitySource (..)
  , resolveIdentity
  , resolveIdentityFrom
  , addressFromSessionId

    -- * Pieces exposed for tests
  , resolveWorkspace
  , mintSessionId
  , findClaudeAncestor
  , claudeAncestorOf
  , HostKey (..)
  , hostKey
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection, Only (..), execute, query)

import Poreus.Effects.Env (CanEnv, getCurrentDir, lookupEnvVar)
import Poreus.Effects.Process (CanProcess)
import Poreus.Effects.Random (CanRandom, randomHex4)
import Poreus.Effects.SystemInfo (CanSystemInfo, getBootId, getMyPid, getParentPid, getProcessName, getProcessStartTime)
import Poreus.Effects.Time (CanTime, currentTime)
import Poreus.Repo (repoRoot)
import Poreus.Time (Timestamp (..))
import Poreus.Types (SessionAddress (..), sessionAddressPrefix)

-- | Where the session id came from. The chain (ADR-0016, which
-- inverted the ADR-0014 precedence): explicit override → persisted
-- host map → host-provided id (seeding the map) → minted (seeding the
-- map). The map is keyed by the Claude host process, so every poreus
-- process spawned by one claude — servers across respawns, hooks —
-- converges on the identity stamped at first contact, no matter how
-- often the host rotates its session id.
data IdentitySource
  = -- | \$POREUS_SESSION_ID (tests, future-proofing); bypasses the map
    SourceEnvOverride
  | -- | host_sessions row keyed by (claude-ancestor pid, boot id,
    -- process start time) — the authoritative identity of a running
    -- claude process after its first contact
    SourceHostMap
  | -- | id handed to us by the host (\$CLAUDE_CODE_SESSION_ID for the
    -- server, the hook's stdin session_id) at first contact; observed,
    -- not documented — never a single point of failure
    SourceClaudeEnv
  | -- | freshly minted at first contact with no host-provided id
    SourceMinted
  deriving stock (Show, Eq)

data Identity = Identity
  { idAddress :: !SessionAddress
  , idSessionId :: !Text
  , idWorkspace :: !Text
  , idSource :: !IdentitySource
  }
  deriving stock (Show, Eq)

addressFromSessionId :: Text -> SessionAddress
addressFromSessionId sid = SessionAddress (sessionAddressPrefix <> sid)

-- | The map key identifying one running Claude host process: ancestor
-- pid (or our own when no claude ancestor is found), the kernel boot
-- id (pids repeat across reboots), and the process start time (pids
-- recycle within one boot; (pid, start time) does not).
data HostKey = HostKey
  { hkPid :: !Int
  , hkBootId :: !Text
  , hkProcStart :: !Integer
  }
  deriving stock (Show, Eq)

hostKey :: CanSystemInfo m => m HostKey
hostKey = do
  myPid <- getMyPid
  ancestor <- findClaudeAncestor
  let pid = fromMaybe myPid ancestor
  boot <- getBootId
  start <- getProcessStartTime pid
  pure (HostKey pid boot (fromMaybe 0 start))

-- | Server-side identity resolution (REG-2): workspace from the
-- environment, host-provided id from \$CLAUDE_CODE_SESSION_ID.
resolveIdentity ::
  (CanEnv m, CanSystemInfo m, CanRandom m, CanTime m, CanProcess m, MonadIO m) =>
  Connection ->
  m Identity
resolveIdentity c = do
  workspace <- resolveWorkspace
  provided <- nonEmptyEnv "CLAUDE_CODE_SESSION_ID"
  resolveIdentityFrom c provided workspace

-- | The shared chain (ADR-0016) used by the server and the hook — the
-- two MUST resolve identically or delivery splits between mailboxes:
--
-- 1. \$POREUS_SESSION_ID — explicit override, bypasses the map.
-- 2. The host_sessions row for this claude process — authoritative
--    once it exists; later rotations of the host-provided id are
--    deliberately ignored (the host re-spawns servers with fresh ids
--    while the original connection keeps serving).
-- 3. The host-provided id (env for the server, stdin session_id for
--    the hook), which seeds the map at first contact.
-- 4. A minted id, also seeding the map.
resolveIdentityFrom ::
  (CanEnv m, CanSystemInfo m, CanRandom m, CanTime m, MonadIO m) =>
  Connection ->
  -- | host-provided session id, if any
  Maybe Text ->
  -- | workspace fact
  Text ->
  m Identity
resolveIdentityFrom c provided workspace = do
  override <- nonEmptyEnv "POREUS_SESSION_ID"
  case override of
    Just sid -> pure (mk sid SourceEnvOverride)
    Nothing -> do
      HostKey{hkPid, hkBootId, hkProcStart} <- hostKey
      now <- Timestamp <$> currentTime
      existing <-
        liftIO $
          query
            c
            "SELECT session_id FROM host_sessions\n\
            \WHERE host_pid = ? AND boot_id = ? AND proc_start = ?"
            (hkPid, hkBootId, hkProcStart)
      case existing of
        (Only sid : _) -> do
          liftIO $
            execute
              c
              "UPDATE host_sessions SET updated_at = ?, workspace = ?\n\
              \WHERE host_pid = ? AND boot_id = ? AND proc_start = ?"
              (now, workspace, hkPid, hkBootId, hkProcStart)
          pure (mk sid SourceHostMap)
        [] -> do
          (sid, src) <- case provided of
            Just sid -> pure (sid, SourceClaudeEnv)
            Nothing -> (,SourceMinted) <$> mintSessionId
          liftIO $
            execute
              c
              "INSERT OR REPLACE INTO host_sessions\n\
              \  (host_pid, boot_id, proc_start, session_id, workspace, updated_at)\n\
              \VALUES (?, ?, ?, ?, ?, ?)"
              (hkPid, hkBootId, hkProcStart, sid, workspace, now)
          pure (mk sid src)
  where
    mk sidT src =
      Identity
        { idAddress = addressFromSessionId sidT
        , idSessionId = sidT
        , idWorkspace = workspace
        , idSource = src
        }

nonEmptyEnv :: CanEnv m => String -> m (Maybe Text)
nonEmptyEnv k = do
  v <- lookupEnvVar k
  pure $ case v of
    Just s | not (null s) -> Just (T.pack s)
    _ -> Nothing

-- | Workspace fact: $CLAUDE_PROJECT_DIR (documented) or the repo root
-- of the server's cwd.
resolveWorkspace :: (CanEnv m, CanProcess m) => m Text
resolveWorkspace = do
  env <- lookupEnvVar "CLAUDE_PROJECT_DIR"
  case env of
    Just p | not (null p) -> pure (T.pack p)
    _ -> do
      cwd <- getCurrentDir
      T.pack <$> repoRoot cwd

-- | 32 lowercase hex chars (128 bits) from the scripted-friendly
-- 16-bit primitive.
mintSessionId :: CanRandom m => m Text
mintSessionId = T.concat <$> mapM (const randomHex4) [1 :: Int .. 8]

-- | Walk the parent chain looking for the Claude Code host process.
-- Bounded to avoid pathological /proc loops.
--
-- The name match tolerates wrapper renaming: NixOS wrapProgram turns
-- the real binary into `.claude-wrapped` (and comm truncates to 15
-- chars, e.g. `.claude-unwrapp`), so leading dots are stripped before
-- the prefix check — a bare "claude" prefix silently never matched on
-- such hosts and the whole map-recovery path was dead.
findClaudeAncestor :: CanSystemInfo m => m (Maybe Int)
findClaudeAncestor = getMyPid >>= claudeAncestorOf

-- | The same walk, starting from an arbitrary pid rather than our own.
--
-- Reading another process's ancestry is how the doorbell names its
-- target: a session's `poreus serve` pid is stored and verified live,
-- and its parent chain reaches the claude window that spawned it. See
-- Note [The claude pid comes from the process tree] in
-- "Poreus.Session".
claudeAncestorOf :: CanSystemInfo m => Int -> m (Maybe Int)
claudeAncestorOf = go (16 :: Int)
  where
    go 0 _ = pure Nothing
    go n pid = do
      mp <- getParentPid pid
      case mp of
        Nothing -> pure Nothing
        Just p | p <= 1 -> pure Nothing
        Just p -> do
          nm <- getProcessName p
          case nm of
            Just name | isClaudeName name -> pure (Just p)
            _ -> go (n - 1) p
    isClaudeName = T.isPrefixOf "claude" . T.dropWhile (== '.')
