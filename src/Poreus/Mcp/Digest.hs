module Poreus.Mcp.Digest
  ( messageDigest
  ) where

import Data.Text (Text)

import Poreus.JSON (jsonToText)
import Poreus.Types

-- | The model-facing rendering of one delivered message: a one-line
-- header naming the sender and the duty, then the full message JSON so
-- the receiving model can act without another round-trip.
--
-- Note [Digest is the hook's, not the channel's]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- This text was built for the `notifications/claude/channel` frames of
-- ADR-0014's layer 3. That layer is withdrawn (ADR-0017) — it was
-- vetoed by org policy on one profile and its emitter thread was dead
-- on the others. The digest itself survives because the hook companion
-- renders exactly the same thing, on the acknowledged path that
-- advances the cursor.
messageDigest :: Message -> Text
messageDigest m =
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
