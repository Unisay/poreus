module Poreus.Repo
  ( repoRoot
  , repoAlias
  , cwdAlias
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode (..))
import System.FilePath (takeFileName, (</>))

import Poreus.Effects.Env (CanEnv, getCurrentDir)
import Poreus.Effects.FileSystem (CanFileSystem, doesFileExist, readFileText)
import Poreus.Effects.Process (CanProcess, runProcessCapture)

-- | Return the repo root for a given path using `git rev-parse`. Falls back
-- to the path itself if `git` fails.
repoRoot :: CanProcess m => FilePath -> m FilePath
repoRoot dir = do
  (ec, out, _err) <- runProcessCapture "git" ["-C", dir, "rev-parse", "--show-toplevel"] ""
  case ec of
    ExitSuccess -> pure (trim out)
    _ -> pure dir
  where
    trim = reverse . dropWhile (`elem` ("\n\r\t " :: String)) . reverse

-- | Alias derived from a directory. If the repo root contains a
-- @.poreus/alias@ file with a non-empty first line, that value is used
-- verbatim. Otherwise, the basename of the repo root is used.
repoAlias :: (CanFileSystem m, CanProcess m) => FilePath -> m Text
repoAlias dir = do
  root <- repoRoot dir
  override <- readAliasOverride (root </> ".poreus" </> "alias")
  case override of
    Just a -> pure a
    Nothing -> pure (T.pack (takeFileName root))

-- | Alias for the current working directory.
cwdAlias :: (CanEnv m, CanFileSystem m, CanProcess m) => m Text
cwdAlias = getCurrentDir >>= repoAlias

-- | Read the first non-empty line from an alias override file, if any.
-- Lines starting with @#@ are treated as comments and skipped. Returns
-- 'Nothing' if the file does not exist, cannot be read, or contains no
-- usable line.
readAliasOverride :: CanFileSystem m => FilePath -> m (Maybe Text)
readAliasOverride path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      res <- readFileText path
      pure $ case res of
        Left _ -> Nothing
        Right body -> firstUsefulLine body

firstUsefulLine :: Text -> Maybe Text
firstUsefulLine body =
  let cleaned = [ stripped
                | raw <- T.lines body
                , let stripped = T.strip raw
                , not (T.null stripped)
                , not ("#" `T.isPrefixOf` stripped)
                ]
   in case cleaned of
        (l : _) -> Just l
        [] -> Nothing
