module Poreus.Server
  ( runServer
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (try)
import Control.Monad (forM_, forever, unless, when)
import Data.Aeson (Value (..))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Database.SQLite.Simple (Connection)
import System.Exit (ExitCode (..))
import System.IO (stdin, stdout)
import qualified System.Posix.Process as Posix
import qualified System.Posix.Signals as Signals

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Poreus.DB (withDB)
import Poreus.Deliver (cursorOf, peekPendingSince)
import Poreus.Effects.SystemInfo (getBootId, getMyPid)
import Poreus.Identity (Identity (..), resolveIdentity)
import Poreus.JSON (encodeLine)
import Poreus.Mcp.Channel (channelNotification)
import Poreus.Mcp.Framing (Transport (..), stdioTransport)
import Poreus.Mcp.JsonRpc (RequestId, incomingId, internalErrorCode, mkRpcError)
import Poreus.Mcp.Protocol (handleLine)
import Poreus.Mcp.Tools (McpEnv (..))
import Poreus.Retention (retentionDays, sweep)
import Poreus.Session (endSession, heartbeat)
import Poreus.Types

-- | The MCP server (ADR-0013): spawned by the host per session over
-- stdio. Owns the JSON-RPC loop and a 5 s tick thread (heartbeat,
-- channel push, hourly retention sweep). One SQLite connection, shared
-- between the two threads behind an MVar.
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
  readyRef <- newIORef False
  -- Channel pushes start at the current cursor: the acknowledged paths
  -- own the backlog; the channel only surfaces what arrives from here on.
  pushedRef <- newIORef =<< cursorOf conn me
  sweepRef <- newIORef =<< getCurrentTime

  let send v = withMVar sendLock (\() -> tSend transport (encodeLine v))
      shutdown = do
        withMVar dbLock (\() -> endSession conn me)
        Posix.exitImmediately ExitSuccess

  -- Graceful shutdown: the host closes stdin on session end; SIGTERM /
  -- SIGINT cover the rest. Either way the session is marked ended and
  -- its name released (REG-3).
  _ <- Signals.installHandler Signals.sigTERM (Signals.Catch shutdown) Nothing
  _ <- Signals.installHandler Signals.sigINT (Signals.Catch shutdown) Nothing

  -- Startup sweep, then hourly from the tick.
  _ <- withMVar dbLock $ \() -> do
    days <- retentionDays
    sweep conn days

  _ <- forkIO (tick conn me dbLock readyRef pushedRef sweepRef send)

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
            when (isInitialize raw) (writeIORef readyRef True)
            loop
  loop
  where
    errorText e = errorCodeText (errCode e) <> ": " <> errMessage e

-- | The 5 s tick: heartbeat (liveness, DISC-4), channel push of
-- messages no acknowledged path has delivered yet (RECV-1 latency
-- bound), and the hourly retention sweep (MAINT-1).
tick ::
  Connection ->
  SessionAddress ->
  MVar () ->
  IORef Bool ->
  IORef Int64 ->
  IORef UTCTime ->
  (Value -> IO ()) ->
  IO ()
tick conn me dbLock readyRef pushedRef sweepRef send = forever $ do
  threadDelay 5_000_000
  withMVar dbLock $ \() -> do
    heartbeat conn me
    ready <- readIORef readyRef
    when ready $ do
      acked <- cursorOf conn me
      pushed <- readIORef pushedRef
      let floor_ = max acked pushed
      msgs <- peekPendingSince conn me floor_
      forM_ msgs (send . channelNotification)
      unless (null msgs) $ writeIORef pushedRef (maximum (map msgSeq msgs))
    lastSweep <- readIORef sweepRef
    now <- getCurrentTime
    when (diffUTCTime now lastSweep > 3600) $ do
      days <- retentionDays
      _ <- sweep conn days
      writeIORef sweepRef now

rawId :: BL.ByteString -> Maybe RequestId
rawId raw = A.decode raw >>= incomingId

isInitialize :: BL.ByteString -> Bool
isInitialize raw = case A.decode raw of
  Just (Object o) -> KM.lookup "method" o == Just (String "initialize")
  _ -> False
