module Poreus.Mcp.JsonRpc
  ( -- * Message shapes
    RequestId (..)
  , Incoming (..)
  , parseIncoming
  , incomingId

    -- * Builders
  , mkResult
  , mkRpcError
  , mkNotification

    -- * Standard error codes
  , parseErrorCode
  , invalidRequestCode
  , methodNotFoundCode
  , invalidParamsCode
  , internalErrorCode
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)

-- | A JSON-RPC request id: opaque, echoed verbatim (numbers and
-- strings both occur in the wild).
newtype RequestId = RequestId {unRequestId :: Value}
  deriving stock (Show, Eq)

-- | One parsed inbound JSON-RPC message. Batches are not supported
-- (ADR-0011): MCP stdio framing is one message per line.
data Incoming
  = IncomingRequest RequestId Text (Maybe Value)
  | IncomingNotification Text (Maybe Value)
  | IncomingInvalid Text
  deriving stock (Show, Eq)

parseIncoming :: Value -> Incoming
parseIncoming = \case
  Object o
    | Just (String method) <- KM.lookup "method" o ->
        let params = KM.lookup "params" o
         in case KM.lookup "id" o of
              Just idv | idv /= Null -> IncomingRequest (RequestId idv) method params
              _ -> IncomingNotification method params
  _ -> IncomingInvalid "not a JSON-RPC request or notification"

-- | Best-effort id extraction from a raw value, for error responses to
-- messages that failed higher-level handling.
incomingId :: Value -> Maybe RequestId
incomingId = \case
  Object o -> case KM.lookup "id" o of
    Just idv | idv /= Null -> Just (RequestId idv)
    _ -> Nothing
  _ -> Nothing

mkResult :: RequestId -> Value -> Value
mkResult (RequestId rid) res =
  object ["jsonrpc" .= ("2.0" :: Text), "id" .= rid, "result" .= res]

mkRpcError :: Maybe RequestId -> Int -> Text -> Value
mkRpcError mrid code message =
  object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "id" .= maybe Null unRequestId mrid
    , "error" .= object ["code" .= code, "message" .= message]
    ]

mkNotification :: Text -> Value -> Value
mkNotification method params =
  object ["jsonrpc" .= ("2.0" :: Text), "method" .= method, "params" .= params]

parseErrorCode, invalidRequestCode, methodNotFoundCode, invalidParamsCode, internalErrorCode :: Int
parseErrorCode = -32700
invalidRequestCode = -32600
methodNotFoundCode = -32601
invalidParamsCode = -32602
internalErrorCode = -32603
