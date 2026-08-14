{-# LANGUAGE FlexibleContexts #-}

-- | In-memory, pure test monad with fake instances of every Can* class.
--
-- `TestM` is a `State` monad over `TestState`, which owns:
--   * a UTC clock (monotonically advancing, caller-controlled)
--   * a deterministic RNG (returns successive values from a list)
--   * an in-memory file system (Map FilePath ByteString)
--   * an environment-variable table
--   * a scripted process table (command+args → (code, stdout, stderr))
--   * a scripted process tree + boot id (for CanSystemInfo)
--
-- Every side-effecting `Poreus.*` function can be exercised inside `TestM`
-- with full determinism, no disk access and no wall-clock dependency.
--
-- For DB-backed tests, use `TestIOM` (`StateT TestState IO`) — it carries
-- the same fake state plus `MonadIO` so you can thread a real SQLite
-- connection in parallel. `withTestDB` gives an isolated `:memory:` DB;
-- `withTestFileDB` gives a shared temp-file DB with two connections for
-- multi-process semantics (takeover, adoption, interleaved cursors).
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
  , withTestDB
  , withTestFileDB

    -- * State
  , TestState (..)
  , ProcInfo (..)
  , emptyTestState
  , initialTestState

    -- * Setters / fixtures
  , setClock
  , advanceClock
  , setRandomInts
  , setEnv
  , unsetEnv
  , addFile
  , addBytes
  , addDir
  , addProcess
  , putProcessDefault
  , setMyPid
  , setBootId
  , addProc

    -- * Observers
  , getFiles
  , getWrites
  , getEnv'
  ) where

import Control.Exception (bracket)
import Control.Monad ((<=<))
import qualified Control.Monad.State.Strict as MS
import qualified Data.ByteString as BS
import Data.List (stripPrefix)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, parseTimeOrError)
import Database.SQLite.Simple (Connection, close, execute_, open)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Poreus.Effects.Env (CanEnv (..))
import Poreus.Effects.FileSystem (CanFileSystem (..))
import Poreus.Effects.Process (CanProcess (..))
import Poreus.Effects.Random (CanRandom (..))
import Poreus.Effects.SystemInfo (CanSystemInfo (..))
import Poreus.Effects.Time (CanTime (..))

import qualified Poreus.DB as DB

-- ---------------------------------------------------------------------
-- State
-- ---------------------------------------------------------------------

-- | One process in the scripted process tree.
data ProcInfo = ProcInfo
  { procParent :: !(Maybe Int)
  , procName :: !Text
  , procAlive :: !Bool
  }

data TestState = TestState
  { tsClock :: !UTCTime
  , tsRandomInts :: ![Int]
  -- ^ RNG is scripted: each call to `randomIntR` consumes the head of
  -- this list (clamped to the requested range); empty list → lo.
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
  , tsMyPid :: !Int
  , tsProcTable :: !(Map Int ProcInfo)
  -- ^ Scripted process tree for CanSystemInfo (parent links, names,
  -- liveness). Pids absent from the table are dead / unknown.
  , tsBootId :: !Text
  }

-- | A state with a fixed epoch (2026-01-01T00:00:00Z), no files, no
-- fixtures. `initialTestState` adds a few defaults that every test
-- usually wants.
emptyTestState :: TestState
emptyTestState =
  TestState
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
    , tsMyPid = 100
    , tsProcTable = Map.empty
    , tsBootId = "boot-test"
    }
  where
    epoch =
      parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Z" "2026-01-01T00:00:00Z"

-- | Convenience fixture: empty state with a scripted list of random
-- integers useful across the suite.
initialTestState :: TestState
initialTestState = emptyTestState{tsRandomInts = cycle [0, 1, 42, 255, 4096, 0xabcd]}

-- ---------------------------------------------------------------------
-- Shared fake implementations (single source for TestM and TestIOM)
-- ---------------------------------------------------------------------

currentTimeS :: MS.MonadState TestState m => m UTCTime
currentTimeS = MS.gets tsClock

randomIntRS :: MS.MonadState TestState m => (Int, Int) -> m Int
randomIntRS bounds = MS.state (drawPureState bounds)

lookupEnvVarS :: MS.MonadState TestState m => String -> m (Maybe String)
lookupEnvVarS k = MS.gets (Map.lookup k . tsEnv)

getHomeDirS :: MS.MonadState TestState m => m FilePath
getHomeDirS = MS.gets tsHomeDir

getCurrentDirS :: MS.MonadState TestState m => m FilePath
getCurrentDirS = MS.gets tsCwd

doesFileExistS :: MS.MonadState TestState m => FilePath -> m Bool
doesFileExistS p = MS.gets (Map.member p . tsFiles)

doesDirectoryExistS :: MS.MonadState TestState m => FilePath -> m Bool
doesDirectoryExistS p = MS.gets (Map.member p . tsDirs)

readFileTextS :: MS.MonadState TestState m => FilePath -> m (Either String Text)
readFileTextS p =
  MS.gets
    ( maybe (Left ("no such file: " <> p)) (Right . TE.decodeUtf8)
        . Map.lookup p
        . tsFiles
    )

readFileBytesS :: MS.MonadState TestState m => FilePath -> m (Either String BS.ByteString)
readFileBytesS p =
  MS.gets (maybe (Left ("no such file: " <> p)) Right . Map.lookup p . tsFiles)

writeFileTextS :: MS.MonadState TestState m => FilePath -> Text -> m ()
writeFileTextS p t = MS.modify $ \s ->
  s
    { tsFiles = Map.insert p (TE.encodeUtf8 t) (tsFiles s)
    , tsWrites = tsWrites s ++ [(p, t)]
    }

listDirectoryS :: MS.MonadState TestState m => FilePath -> m [FilePath]
listDirectoryS p = MS.gets (findChildren p)

createDirectoryIfMissingS :: MS.MonadState TestState m => Bool -> FilePath -> m ()
createDirectoryIfMissingS _ p =
  MS.modify $ \s -> s{tsDirs = Map.insert p () (tsDirs s)}

removeFileS :: MS.MonadState TestState m => FilePath -> m ()
removeFileS p = MS.modify $ \s -> s{tsFiles = Map.delete p (tsFiles s)}

runProcessCaptureS ::
  MS.MonadState TestState m =>
  FilePath ->
  [String] ->
  String ->
  m (ExitCode, String, String)
runProcessCaptureS cmd args _stdin =
  MS.gets $ \s ->
    Map.findWithDefault (tsProcessDefault s) (cmd, args) (tsProcesses s)

getMyPidS :: MS.MonadState TestState m => m Int
getMyPidS = MS.gets tsMyPid

getParentPidS :: MS.MonadState TestState m => Int -> m (Maybe Int)
getParentPidS pid = MS.gets (procParent <=< (Map.lookup pid . tsProcTable))

getProcessNameS :: MS.MonadState TestState m => Int -> m (Maybe Text)
getProcessNameS pid = MS.gets (fmap procName . Map.lookup pid . tsProcTable)

isPidAliveS :: MS.MonadState TestState m => Int -> m Bool
isPidAliveS pid = MS.gets (maybe False procAlive . Map.lookup pid . tsProcTable)

getBootIdS :: MS.MonadState TestState m => m Text
getBootIdS = MS.gets tsBootId

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
  currentTime = currentTimeS

instance CanRandom TestM where
  randomIntR = randomIntRS

instance CanEnv TestM where
  lookupEnvVar = lookupEnvVarS
  getHomeDir = getHomeDirS
  getCurrentDir = getCurrentDirS

instance CanFileSystem TestM where
  doesFileExist = doesFileExistS
  doesDirectoryExist = doesDirectoryExistS
  readFileText = readFileTextS
  readFileBytes = readFileBytesS
  writeFileText = writeFileTextS
  listDirectory = listDirectoryS
  createDirectoryIfMissing = createDirectoryIfMissingS
  removeFile = removeFileS

instance CanProcess TestM where
  runProcessCapture = runProcessCaptureS

instance CanSystemInfo TestM where
  getMyPid = getMyPidS
  getParentPid = getParentPidS
  getProcessName = getProcessNameS
  isPidAlive = isPidAliveS
  getBootId = getBootIdS

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
  currentTime = currentTimeS

instance CanRandom TestIOM where
  randomIntR = randomIntRS

instance CanEnv TestIOM where
  lookupEnvVar = lookupEnvVarS
  getHomeDir = getHomeDirS
  getCurrentDir = getCurrentDirS

instance CanFileSystem TestIOM where
  doesFileExist = doesFileExistS
  doesDirectoryExist = doesDirectoryExistS
  readFileText = readFileTextS
  readFileBytes = readFileBytesS
  writeFileText = writeFileTextS
  listDirectory = listDirectoryS
  createDirectoryIfMissing = createDirectoryIfMissingS
  removeFile = removeFileS

instance CanProcess TestIOM where
  runProcessCapture = runProcessCaptureS

instance CanSystemInfo TestIOM where
  getMyPid = getMyPidS
  getParentPid = getParentPidS
  getProcessName = getProcessNameS
  isPidAlive = isPidAliveS
  getBootId = getBootIdS

-- ---------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------

drawPureState :: (Int, Int) -> TestState -> (Int, TestState)
drawPureState (lo, hi) s =
  case tsRandomInts s of
    (x : xs) ->
      let clamped
            | hi < lo = lo
            | otherwise = lo + (abs x `mod` (hi - lo + 1))
       in (clamped, s{tsRandomInts = xs})
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
stripDir prefix candidate = fromMaybe candidate (stripPrefix prefix candidate)

dedup :: Ord a => [a] -> [a]
dedup = Map.keys . Map.fromList . map (,())

-- ---------------------------------------------------------------------
-- Fixture setters (usable in any MonadState TestState m)
-- ---------------------------------------------------------------------

setClock :: MS.MonadState TestState m => UTCTime -> m ()
setClock t = MS.modify $ \s -> s{tsClock = t}

advanceClock :: MS.MonadState TestState m => Double -> m ()
advanceClock dSecs =
  MS.modify $ \s -> s{tsClock = addUTCTime (realToFrac dSecs) (tsClock s)}

setRandomInts :: MS.MonadState TestState m => [Int] -> m ()
setRandomInts xs = MS.modify $ \s -> s{tsRandomInts = xs}

setEnv :: MS.MonadState TestState m => String -> String -> m ()
setEnv k v = MS.modify $ \s -> s{tsEnv = Map.insert k v (tsEnv s)}

unsetEnv :: MS.MonadState TestState m => String -> m ()
unsetEnv k = MS.modify $ \s -> s{tsEnv = Map.delete k (tsEnv s)}

addFile :: MS.MonadState TestState m => FilePath -> Text -> m ()
addFile p t =
  MS.modify $ \s -> s{tsFiles = Map.insert p (TE.encodeUtf8 t) (tsFiles s)}

addBytes :: MS.MonadState TestState m => FilePath -> BS.ByteString -> m ()
addBytes p bs = MS.modify $ \s -> s{tsFiles = Map.insert p bs (tsFiles s)}

addDir :: MS.MonadState TestState m => FilePath -> m ()
addDir p = MS.modify $ \s -> s{tsDirs = Map.insert p () (tsDirs s)}

addProcess ::
  MS.MonadState TestState m =>
  FilePath ->
  [String] ->
  (ExitCode, String, String) ->
  m ()
addProcess cmd args result =
  MS.modify $ \s -> s{tsProcesses = Map.insert (cmd, args) result (tsProcesses s)}

putProcessDefault :: MS.MonadState TestState m => (ExitCode, String, String) -> m ()
putProcessDefault d = MS.modify $ \s -> s{tsProcessDefault = d}

setMyPid :: MS.MonadState TestState m => Int -> m ()
setMyPid p = MS.modify $ \s -> s{tsMyPid = p}

setBootId :: MS.MonadState TestState m => Text -> m ()
setBootId b = MS.modify $ \s -> s{tsBootId = b}

addProc :: MS.MonadState TestState m => Int -> ProcInfo -> m ()
addProc pid info =
  MS.modify $ \s -> s{tsProcTable = Map.insert pid info (tsProcTable s)}

getFiles :: MS.MonadState TestState m => m (Map FilePath BS.ByteString)
getFiles = MS.gets tsFiles

getWrites :: MS.MonadState TestState m => m [(FilePath, Text)]
getWrites = MS.gets tsWrites

getEnv' :: MS.MonadState TestState m => m (Map String String)
getEnv' = MS.gets tsEnv

-- ---------------------------------------------------------------------
-- SQLite helpers for DB-backed tests
-- ---------------------------------------------------------------------

-- | Open a fresh `:memory:` SQLite DB with foreign keys on, run the
-- schema migration, invoke the action in `TestIOM`, and close the
-- connection afterwards (bracket-safe: the connection closes even if
-- the action throws).
--
-- Each call returns an isolated DB — two tests can run concurrently
-- without sharing any state.
withTestDB :: TestState -> (Connection -> TestIOM a) -> IO (a, TestState)
withTestDB initSt action =
  bracket (openWithPragmas ":memory:") close $ \conn -> do
    DB.migrate conn
    runTestIOM (action conn) initSt

-- | Open a temp-file SQLite DB twice — two independent connections over
-- the same store, mimicking two concurrent poreus processes. Used for
-- multi-process semantics: takeover, adoption, interleaved cursor
-- advance.
withTestFileDB ::
  TestState ->
  (Connection -> Connection -> TestIOM a) ->
  IO (a, TestState)
withTestFileDB initSt action =
  withSystemTempDirectory "poreus-test" $ \dir -> do
    let path = dir </> "db.sqlite"
    bracket (openWithPragmas path) close $ \c1 ->
      bracket (openWithPragmas path) close $ \c2 -> do
        DB.migrate c1
        runTestIOM (action c1 c2) initSt

openWithPragmas :: FilePath -> IO Connection
openWithPragmas path = do
  c <- open path
  execute_ c "PRAGMA foreign_keys = ON"
  execute_ c "PRAGMA busy_timeout = 10000"
  pure c
