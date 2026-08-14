{-# LANGUAGE ConstraintKinds #-}

module Poreus.Mcp.Protocol
  ( -- * Message-level protocol handler
    handleValue
  , handleLine

    -- * Pieces exposed for tests
  , negotiateVersion
  , supportedVersions
  , serverInstructions
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Data.Version (showVersion)

import Paths_poreus (version)
import Poreus.Identity (Identity (..))
import Poreus.Mcp.JsonRpc
import Poreus.Mcp.Tools (McpEnv (..), ToolDef (..), ToolM, runTool, toolDefs)
import Poreus.Session (ensureSession)

-- | Protocol revisions this server understands, newest first. The MCP
-- handshake echoes the client's version when supported, else offers
-- our newest — the client disconnects if it can't live with it.
supportedVersions :: [Text]
supportedVersions = ["2025-06-18", "2025-03-26", "2024-11-05"]

negotiateVersion :: Maybe Text -> Text
negotiateVersion = \case
  Just v | v `elem` supportedVersions -> v
  _ -> head supportedVersions

-- | Injected into the connected session's context by the host. This is
-- where the protocol duty travels (RECV-1: no external skill document
-- needed); receiver *policy* stays in the consumer layer.
serverInstructions :: Text
serverInstructions =
  "poreus ferries messages between the AI agent sessions on this machine. \
  \You are addressable automatically (see whoami); other sessions may send you \
  \free-text requests or typed calls at any time. Incoming messages reach you as \
  \new_messages on poreus tool results, as hook-injected context, and possibly as \
  \<channel source=\"poreus\"> notifications between turns; the same message can \
  \surface twice on different paths — deduplicate by message_id. \
  \THE REPLY DUTY: when a delivered message is a request, always answer it with the \
  \reply tool — exactly one terminal notice (event completed, failed, or aborted) \
  \with a summary, plus started first when the work is more than momentary and stuck \
  \when blocked. Requests you never reply to stay open in the sender's view forever. \
  \To delegate work yourself: discover (who is out there), then call (typed endpoint) \
  \or request (free-text), keep the returned message_id, and check closure later with \
  \messages scope: thread."

-- | Handle one raw frame (already parsed JSON). Returns the outbound
-- frames to write, zero or more. Notifications produce nothing;
-- requests produce exactly one response.
handleValue :: ToolM m => McpEnv -> Value -> m [Value]
handleValue env v = case parseIncoming v of
  IncomingInvalid detail ->
    pure [mkRpcError (incomingId v) invalidRequestCode detail]
  IncomingNotification _ _ ->
    -- notifications/initialized, notifications/cancelled, …: nothing
    -- requires action at this layer.
    pure []
  IncomingRequest rid method params -> case method of
    "initialize" -> do
      let Identity{idAddress, idWorkspace} = envIdentity env
      -- First contact: the session becomes addressable here (REG-2).
      _ <- ensureSession (envConn env) idAddress idWorkspace (envPid env) (envBootId env)
      let requested = case params of
            Just (Object o) -> case KM.lookup "protocolVersion" o of
              Just (String t) -> Just t
              _ -> Nothing
            _ -> Nothing
      pure
        [ mkResult rid $
            object
              [ "protocolVersion" .= negotiateVersion requested
              , "capabilities"
                  .= object
                    [ "tools" .= object []
                    , "experimental" .= object ["claude/channel" .= object []]
                    ]
              , "serverInfo"
                  .= object
                    [ "name" .= ("poreus" :: Text)
                    , "version" .= showVersion version
                    ]
              , "instructions" .= serverInstructions
              ]
        ]
    "ping" -> pure [mkResult rid (object [])]
    "tools/list" ->
      pure
        [ mkResult rid $
            object
              [ "tools"
                  .= [ object
                         [ "name" .= tdName t
                         , "description" .= tdDescription t
                         , "inputSchema" .= tdSchema t
                         ]
                     | t <- toolDefs
                     ]
              ]
        ]
    "tools/call" -> do
      let (mname, args) = case params of
            Just (Object o) ->
              ( case KM.lookup "name" o of
                  Just (String t) -> Just t
                  _ -> Nothing
              , case KM.lookup "arguments" o of
                  Just (Object a) -> a
                  _ -> KM.empty
              )
            _ -> (Nothing, KM.empty)
      case mname of
        Nothing ->
          pure [mkRpcError (Just rid) invalidParamsCode "tools/call requires params.name"]
        Just name -> do
          outcome <- runTool env name args
          pure $ case outcome of
            Nothing ->
              [mkRpcError (Just rid) invalidParamsCode ("unknown tool: " <> name)]
            Just result -> [mkResult rid result]
    _ -> pure [mkRpcError (Just rid) methodNotFoundCode ("method not supported: " <> method)]

-- | Line-level entry: parse one frame; malformed JSON yields a
-- JSON-RPC parse error (transport failures stay JSON-RPC errors,
-- domain failures are tool results — spec §9).
handleLine :: ToolM m => McpEnv -> BL.ByteString -> m [Value]
handleLine env raw = case A.decode raw of
  Nothing -> pure [mkRpcError Nothing parseErrorCode "invalid JSON"]
  Just v -> handleValue env v
