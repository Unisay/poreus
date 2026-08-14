module Poreus.Effects.Process
  ( CanProcess (..)
  ) where

import Control.Exception (SomeException, try)
import Control.Monad.Reader (ReaderT, lift)
import Control.Monad.State.Strict (StateT)
import Control.Monad.Trans.Except (ExceptT)
import System.Exit (ExitCode (..))
import qualified System.Process as Proc

-- | Subprocess execution. Poreus only needs a single, narrow operation:
-- run a command, capture stdout, and get the exit code.
class Monad m => CanProcess m where
  runProcessCapture ::
    -- | command
    FilePath ->
    -- | args
    [String] ->
    -- | stdin
    String ->
    -- | (code, stdout, stderr)
    m (ExitCode, String, String)

instance CanProcess IO where
  -- A missing executable (git absent from PATH, sandboxed build)
  -- reports as a failing exit code, never an exception — callers
  -- treat any failure as "no result" and fall back.
  runProcessCapture cmd args stdin_ = do
    r <- try (Proc.readProcessWithExitCode cmd args stdin_)
    pure $ case r :: Either SomeException (ExitCode, String, String) of
      Right ok -> ok
      Left e -> (ExitFailure 127, "", show e)

instance CanProcess m => CanProcess (ReaderT r m) where
  runProcessCapture cmd args sin_ = lift (runProcessCapture cmd args sin_)

instance CanProcess m => CanProcess (StateT s m) where
  runProcessCapture cmd args sin_ = lift (runProcessCapture cmd args sin_)

instance CanProcess m => CanProcess (ExceptT e m) where
  runProcessCapture cmd args sin_ = lift (runProcessCapture cmd args sin_)
