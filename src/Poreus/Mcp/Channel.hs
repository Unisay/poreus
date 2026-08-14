module Poreus.Mcp.Channel
  ( channelNotification
  , channelDigest
  ) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)

import Poreus.JSON (jsonToText)
import Poreus.Mcp.JsonRpc (mkNotification)
import Poreus.Types

-- | Build one `notifications/claude/channel` frame — the idle wake-up
-- path (OQ-1 answer, ADR-0014). Constraints pinned here:
-- meta keys are underscore-only (hyphenated keys are silently dropped
-- by the host), delivery is best-effort and unacknowledged, so this
-- path never advances the cursor — the server tracks its own pushed
-- floor and a rare channel-then-piggyback duplicate is acceptable
-- (message ids make it recognizable).
channelNotification :: Message -> Value
channelNotification m =
  mkNotification
    "notifications/claude/channel"
    ( object
        [ "content" .= channelDigest m
        , "meta"
            .= object
              [ "message_id" .= msgId m
              , "message_kind" .= msgKind m
              ]
        ]
    )

-- | The human/model-facing content of a channel push: a one-paragraph
-- digest plus the full message JSON, so the receiving model can act
-- without another round-trip.
channelDigest :: Message -> Text
channelDigest m =
  header <> "\n" <> jsonToText m
  where
    header = case msgKind m of
      MKRequest ->
        "Incoming poreus request "
          <> unMessageId (msgId m)
          <> " from "
          <> senderLabel
          <> ". The reply duty applies: answer with the poreus reply tool (exactly one terminal notice: completed/failed/aborted)."
      MKNotice ->
        "Incoming poreus notice "
          <> unMessageId (msgId m)
          <> " from "
          <> senderLabel
          <> maybe "" (\e -> " (event: " <> e <> ")") (messageEvent m)
          <> "."
    senderLabel = case msgFromName m of
      Just n -> unAgentName n <> " (" <> unSessionAddress (msgFrom m) <> ")"
      Nothing -> unSessionAddress (msgFrom m)
