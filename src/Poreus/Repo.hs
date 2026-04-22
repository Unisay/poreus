module Poreus.Repo
  ( repoRoot
  , repoAlias
  , cwdAlias
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode (..))
import System.FilePath (takeFileName)

import Poreus.Effects.Env (CanEnv, getCurrentDir)
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

-- | Alias derived from a directory: basename of its repo root.
repoAlias :: CanProcess m => FilePath -> m Text
repoAlias dir = do
  root <- repoRoot dir
  pure (T.pack (takeFileName root))

-- | Alias for the current working directory.
cwdAlias :: (CanEnv m, CanProcess m) => m Text
cwdAlias = getCurrentDir >>= repoAlias
