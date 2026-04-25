module Poreus.Exit
  ( ExitKind (..)
  , exitJsonError
  , exitCodeOf
  ) where

import Data.Aeson (Value, object, (.=))
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Aeson.Encode.Pretty as AP
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)

data ExitKind
  = ExitGeneric
  | ExitBadArgs
  | ExitNotFound
  | ExitDB
  | ExitFollowAlready  -- ^ inbox -f already running for this Claude session
  | ExitFollowForeign  -- ^ inbox -f held by another session; pass --takeover
  deriving stock (Show, Eq)

exitCodeOf :: ExitKind -> Int
exitCodeOf = \case
  ExitGeneric -> 1
  ExitBadArgs -> 2
  ExitNotFound -> 3
  ExitDB -> 5
  ExitFollowAlready -> 64
  ExitFollowForeign -> 65

exitJsonError :: ExitKind -> Text -> IO a
exitJsonError kind msg = do
  let payload :: Value
      payload =
        object
          [ "error" .= T.pack (show kind)
          , "message" .= msg
          , "code" .= exitCodeOf kind
          ]
  hPutStrLn stderr (BL8.unpack (AP.encodePretty payload))
  exitWith (ExitFailure (exitCodeOf kind))
