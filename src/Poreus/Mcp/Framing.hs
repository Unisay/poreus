module Poreus.Mcp.Framing
  ( Transport (..)
  , stdioTransport
  ) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import System.IO (Handle, hFlush)

-- | The MCP stdio framing (ADR-0011): newline-delimited JSON-RPC, one
-- message per line, no Content-Length headers, no batching. Injected
-- so the protocol loop is testable without any handles.
data Transport m = Transport
  { tRecv :: m (Maybe BL.ByteString)
  -- ^ Next line; Nothing on EOF.
  , tSend :: BL.ByteString -> m ()
  -- ^ Send one already-newline-terminated frame.
  }

-- | Real stdio transport. Reads strict lines (messages are small
-- relative to memory, C-10 caps payloads at ~1 MB) and flushes every
-- write — the host reads responses interactively.
stdioTransport :: Handle -> Handle -> Transport IO
stdioTransport hin hout =
  Transport
    { tRecv = do
        r <- try (BS8.hGetLine hin)
        pure $ case r :: Either SomeException BS8.ByteString of
          Left _ -> Nothing
          Right line -> Just (BL.fromStrict line)
    , tSend = \frame -> do
        BL.hPut hout frame
        hFlush hout
    }
