{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}

-- | In-memory, pure test monad with fake instances of every Can* class.
--
-- `TestM` is a `State` monad over `TestState`, which owns:
--   * a UTC clock (monotonically advancing, caller-controlled)
--   * a deterministic RNG (returns successive values from a list)
--   * an in-memory file system (Map FilePath Text or ByteString)
--   * an environment-variable table
--   * a scripted process table (command+args → (code, stdout, stderr))
--
-- Every side-effecting `Poreus.*` function can be exercised inside `TestM`
-- with full determinism, no disk access and no wall-clock dependency.
--
-- For DB-backed tests, use `TestIOM` (`StateT TestState IO`) — it carries
-- the same fake state plus `MonadIO` so you can thread a real `:memory:`
-- SQLite connection in parallel.
module Poreus.TestM
  ( -- * Pure monad
    TestM
  , runTestM
  , execTestM
  , evalTestM
    -- * Semi-pure monad (adds MonadIO for SQLite)
  , TestIOM
  , runTestIOM
  , execTestIOM
  , evalTestIOM
  , withMemoryDB
    -- * State
  , TestState (..)
  , emptyTestState
  , initialTestState
    -- * Setters / fixtures
  , setClock
  , advanceClock
  , setRandomInts
  , setEnv
  , addFile
  , addBytes
  , addDir
  , addProcess
  , putProcessDefault
    -- * Observers
  , getFiles
  , getWrites
  , getEnv'
  ) where

import qualified Control.Monad.State.Strict as MS
import qualified Data.ByteString as BS
import Data.List (stripPrefix)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, addUTCTime, parseTimeOrError, defaultTimeLocale)
import Database.SQLite.Simple (Connection, close, open)
import System.Exit (ExitCode (..))

import Poreus.Effects.Env (CanEnv (..))
import Poreus.Effects.FileSystem (CanFileSystem (..))
import Poreus.Effects.Process (CanProcess (..))
import Poreus.Effects.Random (CanRandom (..))
import Poreus.Effects.Time (CanTime (..))

import qualified Poreus.DB as DB

-- ---------------------------------------------------------------------
-- State
-- ---------------------------------------------------------------------

data TestState = TestState
  { tsClock :: !UTCTime
  , tsRandomInts :: ![Int]
  -- ^ RNG is scripted: each call to `randomIntR` consumes the head of
  -- this list (clamped to the requested range); empty list → 0.
  , tsFiles :: !(Map FilePath BS.ByteString)
  -- ^ File contents. Text reads decode UTF-8.
  , tsDirs :: !(Map FilePath ())
  -- ^ Directories that exist. We track dirs explicitly because
  -- existence is independent of file content.
  , tsWrites :: ![(FilePath, Text)]
  -- ^ Ordered log of `writeFileText` calls — useful for assertions.
  , tsEnv :: !(Map String String)
  , tsHomeDir :: !FilePath
  , tsCwd :: !FilePath
  , tsProcesses :: !(Map (FilePath, [String]) (ExitCode, String, String))
  , tsProcessDefault :: !(ExitCode, String, String)
  }

-- | A state with a fixed epoch (2026-01-01T00:00:00Z), no files, no
-- fixtures. `initialTestState` adds a few defaults that every test
-- usually wants.
emptyTestState :: TestState
emptyTestState = TestState
  { tsClock = epoch
  , tsRandomInts = []
  , tsFiles = Map.empty
  , tsDirs = Map.empty
  , tsWrites = []
  , tsEnv = Map.empty
  , tsHomeDir = "/home/test"
  , tsCwd = "/home/test"
  , tsProcesses = Map.empty
  , tsProcessDefault = (ExitFailure 1, "", "")
  }
  where
    epoch =
      parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Z" "2026-01-01T00:00:00Z"

-- | Convenience fixture: empty state with HOME and a scripted list of
-- random integers useful across the suite.
initialTestState :: TestState
initialTestState = emptyTestState { tsRandomInts = cycle [0, 1, 42, 255, 4096, 0xabcd] }

-- ---------------------------------------------------------------------
-- TestM — pure, no IO at all
-- ---------------------------------------------------------------------

newtype TestM a = TestM {unTestM :: MS.State TestState a}
  deriving newtype (Functor, Applicative, Monad, MS.MonadState TestState)

runTestM :: TestM a -> TestState -> (a, TestState)
runTestM = MS.runState . unTestM

execTestM :: TestM a -> TestState -> TestState
execTestM = MS.execState . unTestM

evalTestM :: TestM a -> TestState -> a
evalTestM = MS.evalState . unTestM

instance CanTime TestM where
  currentTime = MS.gets tsClock

instance CanRandom TestM where
  randomIntR bounds = drawRandom bounds

instance CanEnv TestM where
  lookupEnvVar k = MS.gets (Map.lookup k . tsEnv)
  getHomeDir = MS.gets tsHomeDir
  getCurrentDir = MS.gets tsCwd

instance CanFileSystem TestM where
  doesFileExist p = MS.gets (Map.member p . tsFiles)
  doesDirectoryExist p = MS.gets (Map.member p . tsDirs)
  readFileText p =
    MS.gets
      ( maybe
          (Left ("no such file: " <> p))
          (Right . TE.decodeUtf8)
          . Map.lookup p
          . tsFiles
      )
  readFileBytes p =
    MS.gets
      ( maybe (Left ("no such file: " <> p)) Right
          . Map.lookup p
          . tsFiles
      )
  writeFileText p t = MS.modify $ \s ->
    s
      { tsFiles = Map.insert p (TE.encodeUtf8 t) (tsFiles s)
      , tsWrites = tsWrites s ++ [(p, t)]
      }
  listDirectory p = MS.gets (findChildren p)
  createDirectoryIfMissing _ p =
    MS.modify $ \s -> s {tsDirs = Map.insert p () (tsDirs s)}

instance CanProcess TestM where
  runProcessCapture cmd args _stdin =
    MS.gets $ \s ->
      Map.findWithDefault (tsProcessDefault s) (cmd, args) (tsProcesses s)

-- ---------------------------------------------------------------------
-- TestIOM — for DB-backed tests
-- ---------------------------------------------------------------------

newtype TestIOM a = TestIOM {unTestIOM :: MS.StateT TestState IO a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadFail
    , MS.MonadState TestState
    , MS.MonadIO
    )

runTestIOM :: TestIOM a -> TestState -> IO (a, TestState)
runTestIOM = MS.runStateT . unTestIOM

execTestIOM :: TestIOM a -> TestState -> IO TestState
execTestIOM = MS.execStateT . unTestIOM

evalTestIOM :: TestIOM a -> TestState -> IO a
evalTestIOM = MS.evalStateT . unTestIOM

instance CanTime TestIOM where
  currentTime = MS.gets tsClock

instance CanRandom TestIOM where
  randomIntR bounds = TestIOM (MS.state (drawPureState bounds))

instance CanEnv TestIOM where
  lookupEnvVar k = MS.gets (Map.lookup k . tsEnv)
  getHomeDir = MS.gets tsHomeDir
  getCurrentDir = MS.gets tsCwd

instance CanFileSystem TestIOM where
  doesFileExist p = MS.gets (Map.member p . tsFiles)
  doesDirectoryExist p = MS.gets (Map.member p . tsDirs)
  readFileText p =
    MS.gets
      ( maybe (Left ("no such file: " <> p)) (Right . TE.decodeUtf8)
          . Map.lookup p
          . tsFiles
      )
  readFileBytes p =
    MS.gets
      ( maybe (Left ("no such file: " <> p)) Right . Map.lookup p . tsFiles
      )
  writeFileText p t = MS.modify $ \s ->
    s
      { tsFiles = Map.insert p (TE.encodeUtf8 t) (tsFiles s)
      , tsWrites = tsWrites s ++ [(p, t)]
      }
  listDirectory p = MS.gets (findChildren p)
  createDirectoryIfMissing _ p =
    MS.modify $ \s -> s {tsDirs = Map.insert p () (tsDirs s)}

instance CanProcess TestIOM where
  runProcessCapture cmd args _stdin =
    MS.gets $ \s ->
      Map.findWithDefault (tsProcessDefault s) (cmd, args) (tsProcesses s)

-- ---------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------

-- | Single source of truth for `randomIntR`. Clamps the head of
-- `tsRandomInts` to the requested range and advances the list.
drawRandom :: (Int, Int) -> TestM Int
drawRandom bounds = TestM (MS.state (drawPureState bounds))

drawPureState :: (Int, Int) -> TestState -> (Int, TestState)
drawPureState (lo, hi) s =
  case tsRandomInts s of
    (x : xs) ->
      let clamped
            | hi < lo = lo
            | otherwise = lo + (abs x `mod` (hi - lo + 1))
       in (clamped, s {tsRandomInts = xs})
    [] -> (lo, s)

-- | Children of a directory in the fake filesystem.
findChildren :: FilePath -> TestState -> [FilePath]
findChildren dir s =
  let prefix = ensureSlash dir
      fromFiles = [stripDir prefix k | k <- Map.keys (tsFiles s), isChild prefix k]
      fromDirs = [stripDir prefix k | k <- Map.keys (tsDirs s), isChild prefix k]
   in dedup (fromFiles ++ fromDirs)

ensureSlash :: FilePath -> FilePath
ensureSlash p
  | null p = "/"
  | last p == '/' = p
  | otherwise = p <> "/"

isChild :: FilePath -> FilePath -> Bool
isChild prefix candidate = case stripPrefix prefix candidate of
  Just rest -> not (null rest) && '/' `notElem` rest
  Nothing -> False

stripDir :: FilePath -> FilePath -> FilePath
stripDir prefix candidate = case stripPrefix prefix candidate of
  Just rest -> rest
  Nothing -> candidate

dedup :: Ord a => [a] -> [a]
dedup = Map.keys . Map.fromList . map (\x -> (x, ()))

-- ---------------------------------------------------------------------
-- Fixture setters (usable in any MonadState TestState m)
-- ---------------------------------------------------------------------

setClock :: MS.MonadState TestState m => UTCTime -> m ()
setClock t = MS.modify $ \s -> s {tsClock = t}

advanceClock :: MS.MonadState TestState m => Double -> m ()
advanceClock dSecs =
  MS.modify $ \s -> s {tsClock = addUTCTime (realToFrac dSecs) (tsClock s)}

setRandomInts :: MS.MonadState TestState m => [Int] -> m ()
setRandomInts xs = MS.modify $ \s -> s {tsRandomInts = xs}

setEnv :: MS.MonadState TestState m => String -> String -> m ()
setEnv k v = MS.modify $ \s -> s {tsEnv = Map.insert k v (tsEnv s)}

addFile :: MS.MonadState TestState m => FilePath -> Text -> m ()
addFile p t =
  MS.modify $ \s -> s {tsFiles = Map.insert p (TE.encodeUtf8 t) (tsFiles s)}

addBytes :: MS.MonadState TestState m => FilePath -> BS.ByteString -> m ()
addBytes p bs = MS.modify $ \s -> s {tsFiles = Map.insert p bs (tsFiles s)}

addDir :: MS.MonadState TestState m => FilePath -> m ()
addDir p = MS.modify $ \s -> s {tsDirs = Map.insert p () (tsDirs s)}

addProcess
  :: MS.MonadState TestState m
  => FilePath
  -> [String]
  -> (ExitCode, String, String)
  -> m ()
addProcess cmd args result =
  MS.modify $ \s -> s {tsProcesses = Map.insert (cmd, args) result (tsProcesses s)}

putProcessDefault :: MS.MonadState TestState m => (ExitCode, String, String) -> m ()
putProcessDefault d = MS.modify $ \s -> s {tsProcessDefault = d}

getFiles :: MS.MonadState TestState m => m (Map FilePath BS.ByteString)
getFiles = MS.gets tsFiles

getWrites :: MS.MonadState TestState m => m [(FilePath, Text)]
getWrites = MS.gets tsWrites

getEnv' :: MS.MonadState TestState m => m (Map String String)
getEnv' = MS.gets tsEnv

-- ---------------------------------------------------------------------
-- In-memory SQLite helper for DB-backed tests
-- ---------------------------------------------------------------------

-- | Open a fresh `:memory:` SQLite DB, run the schema migration, invoke
-- the action in `TestIOM`, and close the connection afterwards.
--
-- Each call returns an isolated DB — two tests can run concurrently
-- without sharing any state.
withMemoryDB :: TestState -> (Connection -> TestIOM a) -> IO (a, TestState)
withMemoryDB initSt action = do
  conn <- open ":memory:"
  DB.migrate conn
  r <- runTestIOM (action conn) initSt
  close conn
  pure r
