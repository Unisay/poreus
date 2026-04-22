module Poreus.Effects.Random
  ( CanRandom (..)
  , randomHex4
  ) where

import Control.Monad.Reader (ReaderT, lift)
import Control.Monad.State.Strict (StateT)
import Control.Monad.Trans.Except (ExceptT)
import Data.Text (Text)
import qualified Data.Text as T
import System.Random (randomRIO)
import Text.Printf (printf)

-- | Effect capability: draw a uniformly random integer in [lo, hi].
--
-- Higher-level helpers in this module build on top of this one primitive
-- so tests only need to provide a single deterministic source.
class Monad m => CanRandom m where
  randomIntR :: (Int, Int) -> m Int

instance CanRandom IO where
  randomIntR = randomRIO

instance CanRandom m => CanRandom (ReaderT r m) where
  randomIntR = lift . randomIntR

instance CanRandom m => CanRandom (StateT s m) where
  randomIntR = lift . randomIntR

instance CanRandom m => CanRandom (ExceptT e m) where
  randomIntR = lift . randomIntR

-- | A 4-digit lowercase hex number, zero-padded. Used as the tail of a task id.
randomHex4 :: CanRandom m => m Text
randomHex4 = do
  n <- randomIntR (0, 0xFFFF)
  pure (T.pack (printf "%04x" n))
