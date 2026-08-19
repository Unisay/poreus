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

-- | The store filename carries the schema generation.
--
-- Note [A new filename instead of a migration]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- v0.4 reshapes `messages` and `cursors` (ADR-0017), and the clean-slate
-- posture of ADR-0006/0009 says no migration. Bumping the filename is
-- what makes the rollout window harmless: a session still running the
-- v0.3 binary keeps writing `db.sqlite` until it exits, instead of
-- meeting a schema it cannot read. The two stores simply do not see
-- each other.
dbPath :: CanEnv m => m FilePath
dbPath = (</> "db-v4.sqlite") <$> poreusHome

ensureHome :: (CanEnv m, CanFileSystem m) => m FilePath
ensureHome = do
  home <- poreusHome
  createDirectoryIfMissing True home
  pure home
