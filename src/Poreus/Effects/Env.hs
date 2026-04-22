module Poreus.Effects.Env
  ( CanEnv (..)
  ) where

import Control.Monad.Reader (ReaderT, lift)
import Control.Monad.State.Strict (StateT)
import Control.Monad.Trans.Except (ExceptT)
import qualified System.Directory as Dir
import qualified System.Environment as Env

-- | Environment access: env vars + a few well-known paths.
class Monad m => CanEnv m where
  lookupEnvVar :: String -> m (Maybe String)
  getHomeDir :: m FilePath
  getCurrentDir :: m FilePath

instance CanEnv IO where
  lookupEnvVar = Env.lookupEnv
  getHomeDir = Dir.getHomeDirectory
  getCurrentDir = Dir.getCurrentDirectory

instance CanEnv m => CanEnv (ReaderT r m) where
  lookupEnvVar = lift . lookupEnvVar
  getHomeDir = lift getHomeDir
  getCurrentDir = lift getCurrentDir

instance CanEnv m => CanEnv (StateT s m) where
  lookupEnvVar = lift . lookupEnvVar
  getHomeDir = lift getHomeDir
  getCurrentDir = lift getCurrentDir

instance CanEnv m => CanEnv (ExceptT e m) where
  lookupEnvVar = lift . lookupEnvVar
  getHomeDir = lift getHomeDir
  getCurrentDir = lift getCurrentDir
