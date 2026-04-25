module Poreus.Lock
  ( -- * Types
    LockHandle
  , Owner (..)
  , AcquireResult (..)
    -- * Acquisition / release
  , acquireFollowLock
  , releaseFollowLock
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, catch)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (removeFile)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO (SeekMode (..))
import System.Posix.IO
  ( LockRequest (..)
  , OpenFileFlags (..)
  , OpenMode (..)
  , closeFd
  , defaultFileFlags
  , openFd
  , setLock
  )
import System.Posix.Process (getProcessID)
import System.Posix.Signals (sigTERM, signalProcess)
import System.Posix.Types (Fd, ProcessID)

import Poreus.Config (poreusHome)
import Poreus.Types (Alias (..))

-- ---------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------

data LockHandle = LockHandle !Fd !FilePath
  -- ^ open fd holding the fcntl lock + path to the pidfile to clean up.

data Owner = Owner
  { ownerPid :: !ProcessID
  , ownerToken :: !Text   -- ^ CLAUDE_CODE_SSE_PORT or "" if unknown.
  }
  deriving stock (Show, Eq)

data AcquireResult
  = Acquired !LockHandle
  | OwnedByMe !Owner    -- ^ session token matches the current process
  | OwnedByOther !Owner -- ^ a different session holds the lock
  deriving stock (Show, Eq)

instance Show LockHandle where
  show (LockHandle _ p) = "LockHandle " <> show p

instance Eq LockHandle where
  LockHandle _ p == LockHandle _ q = p == q

-- ---------------------------------------------------------------------
-- Paths
-- ---------------------------------------------------------------------

-- | `$POREUS_HOME/.inbox-<alias>.lock` — fd we flock on.
lockPath :: Alias -> IO FilePath
lockPath (Alias a) = do
  home <- poreusHome
  pure (home </> (".inbox-" <> T.unpack a <> ".lock"))

-- | `$POREUS_HOME/.inbox-<alias>.pid` — `<pid>\n<token>\n`.
pidPath :: Alias -> IO FilePath
pidPath (Alias a) = do
  home <- poreusHome
  pure (home </> (".inbox-" <> T.unpack a <> ".pid"))

-- ---------------------------------------------------------------------
-- Acquisition
-- ---------------------------------------------------------------------

-- | Try to acquire the per-(alias, session) follow-mode lock. Behaviour:
--
-- 1. Open / create the lock file, attempt fcntl `WriteLock` non-blocking.
-- 2. On success: write `<pid>\n<token>\n` to the pidfile and return
--    `Acquired`.
-- 3. On contention: read the pidfile to identify the holder; if their
--    session token equals ours, return `OwnedByMe` (idempotent
--    no-op). Otherwise:
--    - If `takeover`: SIGTERM the holder, wait up to 3 seconds for
--      the lock to release, then retry once. Return `Acquired` or
--      `OwnedByOther` on timeout.
--    - Else: return `OwnedByOther`.
acquireFollowLock :: Alias -> Bool -> IO AcquireResult
acquireFollowLock alias takeover = do
  lp <- lockPath alias
  pp <- pidPath alias
  fd <- openFd lp ReadWrite (defaultFileFlags { creat = Just 0o644 })
  lockOk <- tryLock fd
  if lockOk
    then claim fd pp
    else do
      mowner <- readOwner pp
      currentToken <- T.pack . fromMaybe "" <$> lookupEnv "CLAUDE_CODE_SSE_PORT"
      case (mowner, takeover) of
        (Just owner, True) -> doTakeover fd pp owner
        (Just owner, False)
          | not (T.null currentToken)
          , ownerToken owner == currentToken -> do
              closeFd fd
              pure (OwnedByMe owner)
        (Just owner, False) -> do
          closeFd fd
          pure (OwnedByOther owner)
        (Nothing, _) -> do
          closeFd fd
          pure (OwnedByOther (Owner 0 ""))
  where
    doTakeover :: Fd -> FilePath -> Owner -> IO AcquireResult
    doTakeover fd pp owner = do
      _ <- (signalProcess sigTERM (ownerPid owner))
        `catch` (\(_ :: IOException) -> pure ())
      ok <- waitForLock fd 30
      if ok
        then claim fd pp
        else do
          closeFd fd
          pure (OwnedByOther owner)

    claim :: Fd -> FilePath -> IO AcquireResult
    claim fd pp = do
      pid <- getProcessID
      token <- T.pack . fromMaybe "" <$> lookupEnv "CLAUDE_CODE_SSE_PORT"
      writePidFile pp pid token
      pure (Acquired (LockHandle fd pp))

-- | Attempt non-blocking `WriteLock` on the whole file. Returns True on
-- success. fcntl signals contention via an IOException (errno EAGAIN
-- or EACCES); catch + return False.
tryLock :: Fd -> IO Bool
tryLock fd = do
  res <- (Right <$> setLock fd (WriteLock, AbsoluteSeek, 0, 0))
    `catch` (\(e :: IOException) -> pure (Left e))
  pure (either (const False) (const True) res)

-- | Poll `tryLock` every 100 ms up to `n` iterations. Returns True if
-- the lock was acquired within the budget.
waitForLock :: Fd -> Int -> IO Bool
waitForLock _ 0 = pure False
waitForLock fd n = do
  ok <- tryLock fd
  if ok
    then pure True
    else do
      threadDelay 100_000
      waitForLock fd (n - 1)

-- ---------------------------------------------------------------------
-- Pidfile I/O
-- ---------------------------------------------------------------------

writePidFile :: FilePath -> ProcessID -> Text -> IO ()
writePidFile pp pid token =
  TIO.writeFile pp (T.pack (show pid) <> "\n" <> token <> "\n")

readOwner :: FilePath -> IO (Maybe Owner)
readOwner pp = do
  res <- (Right <$> TIO.readFile pp)
    `catch` (\(e :: IOException) -> pure (Left e))
  case res of
    Left _ -> pure Nothing
    Right body -> case T.lines body of
      (pidLine : tokLine : _) -> case readPid pidLine of
        Just pid -> pure (Just (Owner pid (T.strip tokLine)))
        Nothing -> pure Nothing
      (pidLine : _) -> case readPid pidLine of
        Just pid -> pure (Just (Owner pid ""))
        Nothing -> pure Nothing
      _ -> pure Nothing

readPid :: Text -> Maybe ProcessID
readPid t = case reads (T.unpack (T.strip t)) :: [(Int, String)] of
  ((n, _) : _) -> Just (fromIntegral n)
  _ -> Nothing

-- ---------------------------------------------------------------------
-- Release
-- ---------------------------------------------------------------------

-- | Best-effort release: unlock + close fd + remove pidfile. Each step
-- catches IO failures so a partial state never blocks the caller.
releaseFollowLock :: LockHandle -> IO ()
releaseFollowLock (LockHandle fd pp) = do
  removeFile pp `catch` (\(_ :: IOException) -> pure ())
  setLock fd (Unlock, AbsoluteSeek, 0, 0)
    `catch` (\(_ :: IOException) -> pure ())
  closeFd fd `catch` (\(_ :: IOException) -> pure ())
