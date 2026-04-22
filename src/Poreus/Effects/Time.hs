module Poreus.Effects.Time
  ( CanTime (..)
  ) where

import Control.Monad.Reader (ReaderT, lift)
import Control.Monad.State.Strict (StateT)
import Control.Monad.Trans.Except (ExceptT)
import Data.Time (UTCTime)
import qualified Data.Time as Time

-- | Effect capability: read the current wall-clock time.
--
-- Real behaviour is wired through the `IO` instance. Tests provide a pure
-- instance (e.g. `StateT` over a fake clock) to drive the time deterministically.
class Monad m => CanTime m where
  currentTime :: m UTCTime

instance CanTime IO where
  currentTime = Time.getCurrentTime

instance CanTime m => CanTime (ReaderT r m) where
  currentTime = lift currentTime

instance CanTime m => CanTime (StateT s m) where
  currentTime = lift currentTime

instance CanTime m => CanTime (ExceptT e m) where
  currentTime = lift currentTime
