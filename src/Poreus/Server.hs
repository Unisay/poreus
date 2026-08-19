module Poreus.Server
  ( runServer
  ) where

import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Exception (try)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import System.Exit (ExitCode (..))
import System.IO (stdin, stdout)
import qualified System.Posix.Process as Posix
import qualified System.Posix.Signals as Signals

import Poreus.DB (withDB)
import Poreus.Effects.SystemInfo (getBootId, getMyPid)
import Poreus.Identity (Identity (..), resolveIdentity)
import Poreus.JSON (encodeLine)
import Poreus.Mcp.Framing (Transport (..), stdioTransport)
import Poreus.Mcp.JsonRpc (RequestId, incomingId, internalErrorCode, mkRpcError)
import Poreus.Mcp.Protocol (handleLine)
import Poreus.Mcp.Tools (McpEnv (..))
import Poreus.Session (endSession)
import Poreus.Types

-- | The MCP server (ADR-0013): spawned by the host per session over
-- stdio. It owns the JSON-RPC loop and nothing else.
--
-- Note [No background threads]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- v0.3 forked a 5 s tick here for heartbeat, channel push and the
-- retention sweep. It was forked bare, so one exception inside
-- `forever` killed all three duties silently while the JSON-RPC loop
-- kept answering — the server looked healthy for 45 h across twelve
-- sessions. ADR-0017 deletes the thread rather than supervising it:
--
--   * heartbeat  → deleted; liveness is computed on read from
--                  (pid, boot_id, proc_start).
--   * channel push → deleted; waking an idle session is the host's
--                  job, via the sender's `SendMessage` doorbell.
--   * retention sweep → moved to the hook path, behind a `last_sweep`
--                  guard.
--
-- Consequence worth keeping in mind when editing: the server is now
-- stateless between calls, so a crash loses nothing, and SQLite
-- writers dropped from "every server, every 5 s" to "on traffic".
-- Do not add a thread back here without an ADR.
runServer :: IO ()
runServer = withDB $ \conn -> do
  identity <- resolveIdentity conn
  pid <- getMyPid
  boot <- getBootId
  let env =
        McpEnv
          { envConn = conn
          , envIdentity = identity
          , envPid = Just pid
          , envBootId = Just boot
          }
      me = idAddress identity
      transport = stdioTransport stdin stdout

  dbLock <- newMVar ()
  sendLock <- newMVar ()

  let send v = withMVar sendLock (\() -> tSend transport (encodeLine v))
      shutdown = do
        withMVar dbLock (\() -> endSession conn me)
        Posix.exitImmediately ExitSuccess

  -- Graceful shutdown: the host closes stdin on session end; SIGTERM /
  -- SIGINT cover the rest. Either way the session is marked ended and
  -- its name released (REG-3).
  _ <- Signals.installHandler Signals.sigTERM (Signals.Catch shutdown) Nothing
  _ <- Signals.installHandler Signals.sigINT (Signals.Catch shutdown) Nothing

  let loop = do
        mline <- tRecv transport
        case mline of
          Nothing -> shutdown
          Just raw -> do
            outcome <- try (withMVar dbLock (\() -> handleLine env raw))
            case outcome of
              Left (PoreusException err) ->
                -- A broken store is a transport-level failure: the
                -- domain result channel may itself be unavailable.
                send (mkRpcError (rawId raw) internalErrorCode (errorText err))
              Right outs -> mapM_ send outs
            loop
  loop
  where
    errorText e = errorCodeText (errCode e) <> ": " <> errMessage e

rawId :: BL.ByteString -> Maybe RequestId
rawId raw = A.decode raw >>= incomingId
