module Poreus.HostSession
  ( -- * The host's own view of a session
    HostSession (..)
  , hostSessionDir
  , hostSessionDirOf
  , hostSessionPathOf
  , readHostSession
  ) where

import Data.Aeson (Value (..))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.FilePath ((</>))
import Text.Read (readMaybe)

import Poreus.Effects.Env (CanEnv, getHomeDir, lookupEnvVar)
import Poreus.Effects.FileSystem (CanFileSystem, readFileText)
import Poreus.Effects.SystemInfo (CanSystemInfo, getProcessEnv)

-- | What Claude Code publishes about one of its own sessions, at
-- @$CLAUDE_CONFIG_DIR/sessions/\<claude-pid\>.json@ (ADR-0017 §5).
--
-- Note [Reading the host session file]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- This is undocumented host state and may move or change shape, so
-- every field is optional and a parse failure is Nothing rather than
-- an error. Reading it was accepted where writing to another session's
-- socket was rejected: reading a status file is passive and
-- idempotent, and the file is deliberately world-readable (0644) while
-- its sibling @.key@ files are 0600.
--
-- `hsVersion` is deliberately absent from this record. The host stamps
-- a version at session start and never rewrites it, so three values
-- were readable on one host at one moment: 2.1.232, 2.1.234, and an
-- installed 2.1.235. A field that is wrong two thirds of the time is
-- worse than no field.
data HostSession = HostSession
  { hsPid :: !(Maybe Int)
  , hsSessionId :: !(Maybe Text)
  , hsName :: !(Maybe Text)
  -- ^ The host's name for the session, as shown in its UI. Changes
  -- mid-session on @\/rename@, which is why callers re-read rather
  -- than snapshot it.
  , hsStatus :: !(Maybe Text)
  , hsStatusUpdatedAt :: !(Maybe Integer)
  -- ^ Milliseconds since the epoch. A value that stops moving while
  -- the pid is alive is what `doctor` reports instead of the deleted
  -- stale-heartbeat check.
  , hsProcStart :: !(Maybe Integer)
  -- ^ Stored by the host as a *string* of clock ticks, not a number.
  , hsCwd :: !(Maybe Text)
  , hsSocketPath :: !(Maybe Text)
  }
  deriving stock (Show, Eq)

-- | OUR OWN profile's sessions directory: @$CLAUDE_CONFIG_DIR@, or
-- @$HOME\/.claude@ when unset.
--
-- Only ever right for our own session. To read somebody else's file,
-- use 'hostSessionDirOf'.
hostSessionDir :: CanEnv m => m FilePath
hostSessionDir = do
  cfg <- lookupEnvVar "CLAUDE_CONFIG_DIR"
  base <- case cfg of
    Just p | not (null p) -> pure p
    _ -> (</> ".claude") <$> getHomeDir
  pure (base </> "sessions")

-- | The sessions directory of the profile a given claude process runs
-- under.
--
-- Note [One store, several host profiles]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A host can run several Claude Code profiles — on this machine,
-- `~/.claude-work` and `~/.claude-personal`. Neither sets
-- `POREUS_HOME`, so both resolve to the same store: ONE poreus
-- database, one role namespace, sessions from both profiles side by
-- side in `sessions`.
--
-- The session files do NOT share a directory. So reading somebody
-- else's file through OUR `CLAUDE_CONFIG_DIR` looks in the wrong
-- profile, and the answer is indistinguishable from "the host publishes
-- nothing for that process". Measured 2026-08-26: `poreus doctor` in
-- the work profile called three live personal-profile sessions broken,
-- while their files sat one directory over the whole time. The doorbell
-- was equally blind to them.
--
-- The config dir is an exec-time environment fact of the claude process
-- itself, so it is read from that process rather than stored — the same
-- move as Note [The claude pid comes from the process tree] in
-- "Poreus.Session", for the same reason. When the environment cannot be
-- read (the process is gone, or the kernel exposes no procfs) this falls
-- back to our own profile, which is the pre-ADR-0019 behaviour and no
-- worse than it.
hostSessionDirOf :: (CanEnv m, CanSystemInfo m) => Int -> m FilePath
hostSessionDirOf pid = do
  theirs <- getProcessEnv pid "CLAUDE_CONFIG_DIR"
  case theirs of
    Just p | not (null p) -> pure (p </> "sessions")
    _ -> hostSessionDir

hostSessionPathOf :: (CanEnv m, CanSystemInfo m) => Int -> m FilePath
hostSessionPathOf pid = (</> (show pid <> ".json")) <$> hostSessionDirOf pid

-- | Read one session's file by the host pid, from that process's own
-- profile. Missing file, unreadable file, or malformed JSON all read as
-- Nothing — a poreus operation must never fail because the host changed
-- its private state layout.
readHostSession ::
  (CanEnv m, CanFileSystem m, CanSystemInfo m) =>
  Int ->
  m (Maybe HostSession)
readHostSession pid = do
  path <- hostSessionPathOf pid
  raw <- readFileText path
  pure $ case raw of
    Left _ -> Nothing
    Right t -> parseHostSession t

parseHostSession :: Text -> Maybe HostSession
parseHostSession t = do
  Object o <- A.decodeStrict' (TE.encodeUtf8 t)
  pure
    HostSession
      { hsPid = intField o "pid"
      , hsSessionId = textField o "sessionId"
      , hsName = textField o "name"
      , hsStatus = textField o "status"
      , hsStatusUpdatedAt = integerField o "statusUpdatedAt"
      , hsProcStart = integerField o "procStart"
      , hsCwd = textField o "cwd"
      , hsSocketPath = textField o "messagingSocketPath"
      }

textField :: A.Object -> Text -> Maybe Text
textField o k = case KM.lookup (Key.fromText k) o of
  Just (String s) -> Just s
  _ -> Nothing

intField :: A.Object -> Text -> Maybe Int
intField o k = fromInteger <$> integerField o k

-- | Tolerates both shapes the host uses: @statusUpdatedAt@ is a JSON
-- number, @procStart@ is a JSON string holding digits.
integerField :: A.Object -> Text -> Maybe Integer
integerField o k = case KM.lookup (Key.fromText k) o of
  Just (Number n) -> Just (truncate (realToFrac n :: Double))
  Just (String s) -> readMaybe (T.unpack s)
  _ -> Nothing
