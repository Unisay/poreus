module Poreus.Identity
  ( -- * Session identity (REG-2)
    Identity (..)
  , IdentitySource (..)
  , resolveIdentity
  , addressFromSessionId

    -- * Pieces exposed for tests
  , resolveWorkspace
  , mintSessionId
  , findClaudeAncestor
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection, Only (..), execute, query)

import Poreus.Effects.Env (CanEnv, getCurrentDir, lookupEnvVar)
import Poreus.Effects.Process (CanProcess)
import Poreus.Effects.Random (CanRandom, randomHex4)
import Poreus.Effects.SystemInfo (CanSystemInfo, getBootId, getMyPid, getParentPid, getProcessName)
import Poreus.Effects.Time (CanTime, currentTime)
import Poreus.Repo (repoRoot)
import Poreus.Time (Timestamp (..))
import Poreus.Types (SessionAddress (..), sessionAddressPrefix)

-- | Where the session id came from. The chain (ADR-0014):
-- explicit override → host env → persisted host map → minted.
data IdentitySource
  = -- | \$POREUS_SESSION_ID (tests, future-proofing)
    SourceEnvOverride
  | -- | \$CLAUDE_CODE_SESSION_ID (observed, not documented — never a
    -- single point of failure)
    SourceClaudeEnv
  | -- | host_sessions row keyed by (claude-ancestor pid, boot id)
    SourceHostMap
  | -- | freshly minted and persisted to host_sessions
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

-- | Resolve who this session is (REG-2): the address is derived from
-- the host's session identifier when available, else recovered from
-- the host_sessions map (so a respawned server inside the same host
-- session reuses its address), else minted. No user action, ever.
resolveIdentity ::
  (CanEnv m, CanSystemInfo m, CanRandom m, CanTime m, CanProcess m, MonadIO m) =>
  Connection ->
  m Identity
resolveIdentity c = do
  workspace <- resolveWorkspace
  override <- nonEmptyEnv "POREUS_SESSION_ID"
  claudeEnv <- nonEmptyEnv "CLAUDE_CODE_SESSION_ID"
  case (override, claudeEnv) of
    (Just sid, _) -> pure (mk sid workspace SourceEnvOverride)
    (_, Just sid) -> pure (mk sid workspace SourceClaudeEnv)
    _ -> do
      myPid <- getMyPid
      ancestor <- findClaudeAncestor
      let hostPid = fromMaybe myPid ancestor
      bootId <- getBootId
      now <- Timestamp <$> currentTime
      existing <-
        liftIO $
          query
            c
            "SELECT session_id FROM host_sessions WHERE host_pid = ? AND boot_id = ?"
            (hostPid, bootId)
      case existing of
        (Only sid : _) -> do
          liftIO $
            execute
              c
              "UPDATE host_sessions SET updated_at = ?, workspace = ? WHERE host_pid = ? AND boot_id = ?"
              (now, workspace, hostPid, bootId)
          pure (mk sid workspace SourceHostMap)
        [] -> do
          sid <- mintSessionId
          liftIO $
            execute
              c
              "INSERT OR REPLACE INTO host_sessions (host_pid, boot_id, session_id, workspace, updated_at)\n\
              \VALUES (?, ?, ?, ?, ?)"
              (hostPid, bootId, sid, workspace, now)
          pure (mk sid workspace SourceMinted)
  where
    mk sidT ws src =
      Identity
        { idAddress = addressFromSessionId sidT
        , idSessionId = sidT
        , idWorkspace = ws
        , idSource = src
        }

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
findClaudeAncestor :: CanSystemInfo m => m (Maybe Int)
findClaudeAncestor = do
  me <- getMyPid
  go (16 :: Int) me
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
            Just name | "claude" `T.isPrefixOf` name -> pure (Just p)
            _ -> go (n - 1) p
