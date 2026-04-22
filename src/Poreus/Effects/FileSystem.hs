module Poreus.Effects.FileSystem
  ( CanFileSystem (..)
  ) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, lift)
import Control.Monad.State.Strict (StateT)
import Control.Monad.Trans.Except (ExceptT)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified System.Directory as Dir

-- | Filesystem operations that Poreus needs: existence checks, text/bytes
-- I/O, and directory traversal. Intentionally narrow — we don't need a
-- full POSIX abstraction.
class Monad m => CanFileSystem m where
  doesFileExist :: FilePath -> m Bool
  doesDirectoryExist :: FilePath -> m Bool
  readFileText :: FilePath -> m (Either String Text)
  readFileBytes :: FilePath -> m (Either String BS.ByteString)
  writeFileText :: FilePath -> Text -> m ()
  listDirectory :: FilePath -> m [FilePath]
  createDirectoryIfMissing :: Bool -> FilePath -> m ()

instance CanFileSystem IO where
  doesFileExist = Dir.doesFileExist
  doesDirectoryExist = Dir.doesDirectoryExist
  readFileText p = tryShow (TIO.readFile p)
  readFileBytes p = tryShow (BS.readFile p)
  writeFileText = TIO.writeFile
  listDirectory p = either (const []) id <$> tryAny (Dir.listDirectory p)
  createDirectoryIfMissing = Dir.createDirectoryIfMissing

tryAny :: IO a -> IO (Either SomeException a)
tryAny = try

tryShow :: IO a -> IO (Either String a)
tryShow = fmap (either (Left . show) Right) . tryAny

instance CanFileSystem m => CanFileSystem (ReaderT r m) where
  doesFileExist = lift . doesFileExist
  doesDirectoryExist = lift . doesDirectoryExist
  readFileText = lift . readFileText
  readFileBytes = lift . readFileBytes
  writeFileText p t = lift (writeFileText p t)
  listDirectory = lift . listDirectory
  createDirectoryIfMissing p b = lift (createDirectoryIfMissing p b)

instance CanFileSystem m => CanFileSystem (StateT s m) where
  doesFileExist = lift . doesFileExist
  doesDirectoryExist = lift . doesDirectoryExist
  readFileText = lift . readFileText
  readFileBytes = lift . readFileBytes
  writeFileText p t = lift (writeFileText p t)
  listDirectory = lift . listDirectory
  createDirectoryIfMissing p b = lift (createDirectoryIfMissing p b)

instance CanFileSystem m => CanFileSystem (ExceptT e m) where
  doesFileExist = lift . doesFileExist
  doesDirectoryExist = lift . doesDirectoryExist
  readFileText = lift . readFileText
  readFileBytes = lift . readFileBytes
  writeFileText p t = lift (writeFileText p t)
  listDirectory = lift . listDirectory
  createDirectoryIfMissing p b = lift (createDirectoryIfMissing p b)

-- MonadIO is unused here but kept to silence warnings about the import
-- in case we want a default impl using it later.
_unusedLiftIO :: MonadIO m => IO a -> m a
_unusedLiftIO = liftIO
