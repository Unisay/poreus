module Poreus.HostSession
  ( -- * The host's own view of a session
    HostSession (..)
  , hostSessionDir
  , hostSessionPath
  , readHostSession
  , listHostSessions
  ) where

import Data.Aeson (Value (..))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.List (isSuffixOf, sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.FilePath (takeBaseName, (</>))
import Text.Read (readMaybe)

import Poreus.Effects.Env (CanEnv, getHomeDir, lookupEnvVar)
import Poreus.Effects.FileSystem (CanFileSystem, listDirectory, readFileText)

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

-- | @$CLAUDE_CONFIG_DIR@, or @$HOME\/.claude@ when unset.
hostSessionDir :: CanEnv m => m FilePath
hostSessionDir = do
  cfg <- lookupEnvVar "CLAUDE_CONFIG_DIR"
  base <- case cfg of
    Just p | not (null p) -> pure p
    _ -> (</> ".claude") <$> getHomeDir
  pure (base </> "sessions")

hostSessionPath :: CanEnv m => Int -> m FilePath
hostSessionPath pid = (</> (show pid <> ".json")) <$> hostSessionDir

-- | Read one session's file by the host pid. Missing file, unreadable
-- file, or malformed JSON all read as Nothing — a poreus operation
-- must never fail because the host changed its private state layout.
readHostSession :: (CanEnv m, CanFileSystem m) => Int -> m (Maybe HostSession)
readHostSession pid = do
  path <- hostSessionPath pid
  raw <- readFileText path
  pure $ case raw of
    Left _ -> Nothing
    Right t -> parseHostSession t

-- | Every session file the host currently publishes, paired with the
-- pid its filename names, lowest pid first. Used by `doctor` to spot
-- poreus rows whose process the host no longer knows about.
listHostSessions :: (CanEnv m, CanFileSystem m) => m [(Int, HostSession)]
listHostSessions = do
  dir <- hostSessionDir
  entries <- listDirectory dir
  -- Sorted by pid so `doctor` prints the same order twice in a row.
  -- The sibling `.key` files are 0600 and are not session files; the
  -- suffix check drops them.
  let pids =
        sort
          [ pid
          | e <- entries
          , ".json" `isSuffixOf` e
          , Just pid <- [readMaybe (takeBaseName e)]
          ]
  concat <$> mapM one pids
  where
    one pid = do
      mhs <- readHostSession pid
      pure [(pid, hs) | Just hs <- [mhs]]

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
