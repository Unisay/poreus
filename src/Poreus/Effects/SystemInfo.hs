module Poreus.Effects.SystemInfo
  ( CanSystemInfo (..)
  ) where

import Control.Exception (SomeException, try)
import Control.Monad.Reader (ReaderT, lift)
import Control.Monad.State.Strict (StateT)
import Control.Monad.Trans.Except (ExceptT)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified System.Posix.Process as Posix
import qualified System.Posix.Signals as Signals

-- | Process/host introspection: own pid, the parent chain (for the
-- claude-ancestor walk in the identity fallback), pid liveness (for
-- name-binding liveness corroboration), and the kernel boot id (pids
-- recycle across reboots; (pid, boot_id) does not).
--
-- The IO instance reads procfs where available and degrades gracefully
-- (Nothing / "unknown") elsewhere — callers must tolerate that.
class Monad m => CanSystemInfo m where
  getMyPid :: m Int
  getParentPid :: Int -> m (Maybe Int)
  getProcessName :: Int -> m (Maybe Text)
  isPidAlive :: Int -> m Bool
  getBootId :: m Text

  -- | Kernel start time of a process (clock ticks since boot,
  -- /proc/<pid>/stat field 22). (pid, boot id, start time) is globally
  -- unique — it survives pid recycling within one boot.
  getProcessStartTime :: Int -> m (Maybe Integer)

  -- | One variable from ANOTHER process's environment.
  --
  -- Note [Reading another process's environment]
  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  -- /proc/<pid>/environ is readable for a process of the same user and
  -- is a snapshot of the environment at exec time — which is exactly
  -- right for `CLAUDE_CONFIG_DIR`, a value the launcher sets once and
  -- the process never changes.
  --
  -- It is read as BYTES and split on NUL before anything is decoded, so
  -- one undecodable neighbouring variable cannot take the whole
  -- environment down with it. That failure would be silent and would
  -- point the wrong way: a missing value reads as \"this process has no
  -- such variable\", which is indistinguishable from the answer being
  -- genuinely absent.
  getProcessEnv :: Int -> String -> m (Maybe String)

instance CanSystemInfo IO where
  getMyPid = fromIntegral <$> Posix.getProcessID

  -- /proc/<pid>/status has "PPid:\t<n>"; absent or unparseable → Nothing.
  getParentPid pid = do
    r <- tryRead ("/proc/" <> show pid <> "/status")
    pure $ do
      body <- r
      line <- lookupLine "PPid:" body
      case reads (dropWhile (`elem` ("\t " :: String)) line) of
        [(n, _)] -> Just n
        _ -> Nothing

  getProcessName pid = do
    r <- tryRead ("/proc/" <> show pid <> "/comm")
    pure (T.strip . T.pack <$> r)

  -- Signal 0 probes existence without delivering anything.
  isPidAlive pid = do
    r <- try (Signals.signalProcess Signals.nullSignal (fromIntegral pid))
    pure $ case r :: Either SomeException () of
      Right () -> True
      Left _ -> False

  getBootId = do
    r <- tryRead "/proc/sys/kernel/random/boot_id"
    pure (maybe "unknown" (T.strip . T.pack) r)

  -- Field 22 of /proc/<pid>/stat. The comm field (2) may contain
  -- spaces and parentheses, so split after the LAST ')': the remainder
  -- starts at field 3 (state), making starttime its 20th word.
  getProcessStartTime pid = do
    r <- tryRead ("/proc/" <> show pid <> "/stat")
    pure $ do
      body <- r
      let afterComm = reverse (takeWhile (/= ')') (reverse body))
      w <- case drop 19 (words afterComm) of
        (x : _) -> Just x
        [] -> Nothing
      case reads w of
        [(n, "")] -> Just n
        _ -> Nothing

  getProcessEnv pid key = do
    r <- tryReadBytes ("/proc/" <> show pid <> "/environ")
    pure $ do
      body <- r
      let prefix = BS8.pack (key <> "=")
      entry <- listToMaybe [e | e <- BS8.split '\0' body, prefix `BS.isPrefixOf` e]
      pure (T.unpack (TE.decodeUtf8Lenient (BS.drop (BS.length prefix) entry)))

tryRead :: FilePath -> IO (Maybe String)
tryRead p = do
  r <- try (readFile p)
  pure $ case r :: Either SomeException String of
    Right s -> Just s
    Left _ -> Nothing

-- | Bytes, not text: see Note [Reading another process's environment].
tryReadBytes :: FilePath -> IO (Maybe BS.ByteString)
tryReadBytes p = do
  r <- try (BS.readFile p)
  pure $ case r :: Either SomeException BS.ByteString of
    Right b -> Just b
    Left _ -> Nothing

lookupLine :: String -> String -> Maybe String
lookupLine prefix body =
  case [drop (length prefix) l | l <- lines body, prefix == take (length prefix) l] of
    (x : _) -> Just x
    [] -> Nothing

instance CanSystemInfo m => CanSystemInfo (ReaderT r m) where
  getMyPid = lift getMyPid
  getParentPid = lift . getParentPid
  getProcessName = lift . getProcessName
  isPidAlive = lift . isPidAlive
  getBootId = lift getBootId
  getProcessStartTime = lift . getProcessStartTime
  getProcessEnv pid = lift . getProcessEnv pid

instance CanSystemInfo m => CanSystemInfo (StateT s m) where
  getMyPid = lift getMyPid
  getParentPid = lift . getParentPid
  getProcessName = lift . getProcessName
  isPidAlive = lift . isPidAlive
  getBootId = lift getBootId
  getProcessStartTime = lift . getProcessStartTime
  getProcessEnv pid = lift . getProcessEnv pid

instance CanSystemInfo m => CanSystemInfo (ExceptT e m) where
  getMyPid = lift getMyPid
  getParentPid = lift . getParentPid
  getProcessName = lift . getProcessName
  isPidAlive = lift . isPidAlive
  getBootId = lift getBootId
  getProcessStartTime = lift . getProcessStartTime
  getProcessEnv pid = lift . getProcessEnv pid
