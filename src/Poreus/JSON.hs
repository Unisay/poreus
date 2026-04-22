module Poreus.JSON
  ( emitJSON
  , prettyConfig
  ) where

import Data.Aeson (ToJSON)
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.ByteString.Lazy.Char8 as BL8

prettyConfig :: AP.Config
prettyConfig =
  AP.defConfig
    { AP.confIndent = AP.Spaces 2
    , AP.confCompare = compare
    , AP.confTrailingNewline = True
    }

emitJSON :: ToJSON a => a -> IO ()
emitJSON = BL8.putStr . AP.encodePretty' prettyConfig
