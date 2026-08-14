module Poreus.Config
  ( poreusHome
  , dbPath
  , ensureHome
  ) where

import System.FilePath ((</>))

import Poreus.Effects.Env (CanEnv, getHomeDir, lookupEnvVar)
import Poreus.Effects.FileSystem (CanFileSystem, createDirectoryIfMissing)

-- | \$POREUS_HOME or ${XDG_DATA_HOME:-$HOME/.local/share}/poreus.
poreusHome :: CanEnv m => m FilePath
poreusHome = do
  env <- lookupEnvVar "POREUS_HOME"
  case env of
    Just p | not (null p) -> pure p
    _ -> do
      xdg <- lookupEnvVar "XDG_DATA_HOME"
      base <- case xdg of
        Just p | not (null p) -> pure p
        _ -> do
          home <- getHomeDir
          pure (home </> ".local" </> "share")
      pure (base </> "poreus")

dbPath :: CanEnv m => m FilePath
dbPath = (</> "db.sqlite") <$> poreusHome

ensureHome :: (CanEnv m, CanFileSystem m) => m FilePath
ensureHome = do
  home <- poreusHome
  createDirectoryIfMissing True home
  pure home
