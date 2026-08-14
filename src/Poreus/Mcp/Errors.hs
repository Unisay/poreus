module Poreus.Mcp.Errors
  ( toolSuccess
  , toolFailure
  ) where

import Data.Aeson (ToJSON (..), Value, object, (.=))
import Data.Text (Text)

import Poreus.JSON (jsonToText)
import Poreus.Types (PoreusError)

-- | Wrap a successful tool payload as an MCP tool result: a compact
-- text rendering (every client understands it) plus structuredContent
-- for hosts that consume it directly.
toolSuccess :: Value -> Value
toolSuccess v =
  object
    [ "content" .= [textContent (jsonToText v)]
    , "structuredContent" .= v
    ]

-- | Domain failures are tool-level errors (spec §9): `isError: true`
-- with the structured {code, message, action} record. JSON-RPC errors
-- stay reserved for transport/shape failures.
toolFailure :: PoreusError -> Value
toolFailure e =
  object
    [ "content" .= [textContent (jsonToText (toJSON e))]
    , "structuredContent" .= toJSON e
    , "isError" .= True
    ]

textContent :: Text -> Value
textContent t = object ["type" .= ("text" :: Text), "text" .= t]
