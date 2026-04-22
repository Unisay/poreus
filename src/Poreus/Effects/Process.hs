module Poreus.Effects.Process
  ( CanProcess (..)
  ) where

import Control.Monad.Reader (ReaderT, lift)
import Control.Monad.State.Strict (StateT)
import Control.Monad.Trans.Except (ExceptT)
import System.Exit (ExitCode)
import qualified System.Process as Proc

-- | Subprocess execution. Poreus only needs a single, narrow operation:
-- run a command, capture stdout, and get the exit code.
class Monad m => CanProcess m where
  runProcessCapture
    :: FilePath -- ^ command
    -> [String] -- ^ args
    -> String   -- ^ stdin
    -> m (ExitCode, String, String) -- ^ (code, stdout, stderr)

instance CanProcess IO where
  runProcessCapture = Proc.readProcessWithExitCode

instance CanProcess m => CanProcess (ReaderT r m) where
  runProcessCapture cmd args sin_ = lift (runProcessCapture cmd args sin_)

instance CanProcess m => CanProcess (StateT s m) where
  runProcessCapture cmd args sin_ = lift (runProcessCapture cmd args sin_)

instance CanProcess m => CanProcess (ExceptT e m) where
  runProcessCapture cmd args sin_ = lift (runProcessCapture cmd args sin_)
