module Poreus.JSON
  ( emitJSON
  , prettyConfig
  , encodeLine
  , jsonToText
  , textToJson
  ) where

import Data.Aeson (ToJSON)
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

prettyConfig :: AP.Config
prettyConfig =
  AP.defConfig
    { AP.confIndent = AP.Spaces 2
    , AP.confCompare = compare
    , AP.confTrailingNewline = True
    }

-- | Pretty JSON to stdout — the admin-command output surface.
emitJSON :: ToJSON a => a -> IO ()
emitJSON = BL8.putStr . AP.encodePretty' prettyConfig

-- | Compact single-line encoding with a trailing newline — the MCP
-- stdio framing unit (one JSON-RPC message per line, ADR-0011).
encodeLine :: ToJSON a => a -> BL.ByteString
encodeLine v = A.encode v <> "\n"

-- | Compact JSON as Text — the storage representation of payloads and
-- tag lists (TEXT columns).
jsonToText :: ToJSON a => a -> Text
jsonToText = TE.decodeUtf8 . BL.toStrict . A.encode

-- | Inverse of 'jsonToText' for reading stored columns back.
textToJson :: Text -> Maybe A.Value
textToJson = A.decodeStrict' . TE.encodeUtf8
