module Poreus.Effects.FileSystem
  ( CanFileSystem (..)
  ) where

import Control.Exception (SomeException, try)
import Control.Monad.Reader (ReaderT, lift)
import Control.Monad.State.Strict (StateT)
import Control.Monad.Trans.Except (ExceptT)
import qualified Data.ByteString as BS
import Data.Either (fromRight)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified System.Directory as Dir

-- | Filesystem operations that Poreus needs: existence checks, text/bytes
-- I/O, directory traversal, and removal. Intentionally narrow — we don't
-- need a full POSIX abstraction.
class Monad m => CanFileSystem m where
  doesFileExist :: FilePath -> m Bool
  doesDirectoryExist :: FilePath -> m Bool

  -- | Size in bytes, or Nothing when the file is absent or unreadable.
  -- Used by `doctor` to see the write-ahead log grow without reading
  -- it — a stalled retention sweep showed up first as a 4.1 MB WAL.
  getFileSize :: FilePath -> m (Maybe Integer)

  readFileText :: FilePath -> m (Either String Text)
  readFileBytes :: FilePath -> m (Either String BS.ByteString)
  writeFileText :: FilePath -> Text -> m ()
  listDirectory :: FilePath -> m [FilePath]
  createDirectoryIfMissing :: Bool -> FilePath -> m ()
  removeFile :: FilePath -> m ()

instance CanFileSystem IO where
  doesFileExist = Dir.doesFileExist
  doesDirectoryExist = Dir.doesDirectoryExist
  getFileSize p = either (const Nothing) Just <$> tryAny (Dir.getFileSize p)
  readFileText p = tryShow (TIO.readFile p)
  readFileBytes p = tryShow (BS.readFile p)
  writeFileText = TIO.writeFile
  listDirectory p = fromRight [] <$> tryAny (Dir.listDirectory p)
  createDirectoryIfMissing = Dir.createDirectoryIfMissing

  -- Best-effort: removing an already-absent file is not an error.
  removeFile p = fromRight () <$> tryAny (Dir.removeFile p)

tryAny :: IO a -> IO (Either SomeException a)
tryAny = try

tryShow :: IO a -> IO (Either String a)
tryShow = fmap (either (Left . show) Right) . tryAny

instance CanFileSystem m => CanFileSystem (ReaderT r m) where
  doesFileExist = lift . doesFileExist
  doesDirectoryExist = lift . doesDirectoryExist
  getFileSize = lift . getFileSize
  readFileText = lift . readFileText
  readFileBytes = lift . readFileBytes
  writeFileText p t = lift (writeFileText p t)
  listDirectory = lift . listDirectory
  createDirectoryIfMissing p b = lift (createDirectoryIfMissing p b)
  removeFile = lift . removeFile

instance CanFileSystem m => CanFileSystem (StateT s m) where
  doesFileExist = lift . doesFileExist
  doesDirectoryExist = lift . doesDirectoryExist
  getFileSize = lift . getFileSize
  readFileText = lift . readFileText
  readFileBytes = lift . readFileBytes
  writeFileText p t = lift (writeFileText p t)
  listDirectory = lift . listDirectory
  createDirectoryIfMissing p b = lift (createDirectoryIfMissing p b)
  removeFile = lift . removeFile

instance CanFileSystem m => CanFileSystem (ExceptT e m) where
  doesFileExist = lift . doesFileExist
  doesDirectoryExist = lift . doesDirectoryExist
  getFileSize = lift . getFileSize
  readFileText = lift . readFileText
  readFileBytes = lift . readFileBytes
  writeFileText p t = lift (writeFileText p t)
  listDirectory = lift . listDirectory
  createDirectoryIfMissing p b = lift (createDirectoryIfMissing p b)
  removeFile = lift . removeFile
