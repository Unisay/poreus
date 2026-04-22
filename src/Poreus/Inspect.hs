module Poreus.Inspect
  ( inspectRepo
  , InspectResult (..)
  ) where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.List (isSuffixOf, sort)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (takeFileName, (</>))

import Poreus.Effects.FileSystem
  ( CanFileSystem
  , doesDirectoryExist
  , doesFileExist
  , listDirectory
  , readFileText
  )
import Poreus.Effects.Process (CanProcess)
import Poreus.Repo (repoRoot)

data InspectResult = InspectResult
  { irRepoRoot :: !FilePath
  , irBasename :: !Text
  , irHasCabal :: !Bool
  , irHasFlake :: !Bool
  , irHasPackageJson :: !Bool
  , irClaudeMdExcerpt :: !(Maybe Text)
  , irSkills :: ![Text]
  , irCommands :: ![Text]
  }
  deriving stock (Show, Eq)

instance ToJSON InspectResult where
  toJSON r =
    object
      [ "repo_root" .= irRepoRoot r
      , "basename" .= irBasename r
      , "has_cabal" .= irHasCabal r
      , "has_flake" .= irHasFlake r
      , "has_package_json" .= irHasPackageJson r
      , "claude_md_excerpt" .= irClaudeMdExcerpt r
      , "skills" .= irSkills r
      , "commands" .= irCommands r
      ]

inspectRepo :: (CanFileSystem m, CanProcess m) => FilePath -> m InspectResult
inspectRepo dir = do
  root <- repoRoot dir
  let base = T.pack (takeFileName root)
  hasCabal <- anyCabal root
  hasFlake <- doesFileExist (root </> "flake.nix")
  hasPkg <- doesFileExist (root </> "package.json")
  claude <- readExcerpt (root </> "CLAUDE.md") 80
  skills <- listSubdirs (root </> "skills")
  commands <- listCommands (root </> "commands")
  pure
    InspectResult
      { irRepoRoot = root
      , irBasename = base
      , irHasCabal = hasCabal
      , irHasFlake = hasFlake
      , irHasPackageJson = hasPkg
      , irClaudeMdExcerpt = claude
      , irSkills = skills
      , irCommands = commands
      }

anyCabal :: CanFileSystem m => FilePath -> m Bool
anyCabal root = do
  direct <- doesFileExist (root </> "cabal.project")
  if direct
    then pure True
    else do
      entries <- listDirectory root
      pure (any (".cabal" `isSuffixOf`) entries)

readExcerpt :: CanFileSystem m => FilePath -> Int -> m (Maybe Text)
readExcerpt path n = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      res <- readFileText path
      case res of
        Left _ -> pure Nothing
        Right t -> pure (Just (T.unlines (take n (T.lines t))))

listSubdirs :: CanFileSystem m => FilePath -> m [Text]
listSubdirs dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure []
    else do
      entries <- listDirectory dir
      dirs <- filterM (doesDirectoryExist . (dir </>)) entries
      pure (sort (map T.pack dirs))

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM _ [] = pure []
filterM p (x : xs) = do
  keep <- p x
  rest <- filterM p xs
  pure (if keep then x : rest else rest)

listCommands :: CanFileSystem m => FilePath -> m [Text]
listCommands dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure []
    else do
      tops <- listDirectory dir
      fmap sort (concatMapM (classify dir) tops)
  where
    concatMapM f xs = fmap concat (mapM f xs)

classify :: CanFileSystem m => FilePath -> FilePath -> m [Text]
classify root entry = do
  let full = root </> entry
  isDir <- doesDirectoryExist full
  if isDir
    then do
      sub <- listDirectory full
      pure [T.pack (entry <> ":" <> stripMd s) | s <- sub, isMd s]
    else
      if isMd entry
        then pure [T.pack (stripMd entry)]
        else pure []

isMd :: String -> Bool
isMd = (".md" `isSuffixOf`)

stripMd :: String -> String
stripMd s
  | ".md" `isSuffixOf` s = take (length s - 3) s
  | otherwise = s
