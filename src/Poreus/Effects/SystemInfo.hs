module Poreus.Effects.SystemInfo
  ( CanSystemInfo (..)
  ) where

import Control.Exception (SomeException, try)
import Control.Monad.Reader (ReaderT, lift)
import Control.Monad.State.Strict (StateT)
import Control.Monad.Trans.Except (ExceptT)
import Data.Text (Text)
import qualified Data.Text as T
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

tryRead :: FilePath -> IO (Maybe String)
tryRead p = do
  r <- try (readFile p)
  pure $ case r :: Either SomeException String of
    Right s -> Just s
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

instance CanSystemInfo m => CanSystemInfo (StateT s m) where
  getMyPid = lift getMyPid
  getParentPid = lift . getParentPid
  getProcessName = lift . getProcessName
  isPidAlive = lift . isPidAlive
  getBootId = lift getBootId

instance CanSystemInfo m => CanSystemInfo (ExceptT e m) where
  getMyPid = lift getMyPid
  getParentPid = lift . getParentPid
  getProcessName = lift . getProcessName
  isPidAlive = lift . isPidAlive
  getBootId = lift getBootId
