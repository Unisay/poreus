module Poreus.Hook
  ( runHook

    -- * Pure pieces exposed for tests
  , HookInput (..)
  , parseHookInput
  , renderDigest
  , renderSuggestion
  , hookOutput
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (for_)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Poreus.DB (withDB)
import Poreus.Deliver (Delivered (..), deliverPending)
import Poreus.Identity (addressFromSessionId)
import Poreus.JSON (jsonToText)
import Poreus.Mcp.Channel (channelDigest)
import Poreus.Name (suggestRoleName)
import Poreus.Session (ensureSession)
import Poreus.Types (AgentName (..), Message)

-- | What the Claude Code hook protocol hands us on stdin. Only three
-- fields matter; the rest of the record is ignored.
data HookInput = HookInput
  { hiSessionId :: !Text
  , hiCwd :: !Text
  , hiEventName :: !Text
  }
  deriving stock (Show, Eq)

parseHookInput :: BL.ByteString -> Maybe HookInput
parseHookInput raw = do
  Object o <- A.decode raw
  String sid <- KM.lookup "session_id" o
  let textField k = case KM.lookup k o of
        Just (String t) -> t
        _ -> ""
  pure
    HookInput
      { hiSessionId = sid
      , hiCwd = textField "cwd"
      , hiEventName = textField "hook_event_name"
      }

-- | The short-lived hook companion (ADR-0013/0014): reads the hook
-- record from stdin, delivers pending messages for the session as
-- context, exits 0 — always, so a poreus hiccup never breaks the
-- user's session. The hook shares the session address with the server
-- by construction: both derive it from the same session id.
--
-- On SessionStart it additionally surfaces the role nudge (the
-- session-start half of the fail-fast on missing names): when the
-- session holds no name and the workspace-derived role is available,
-- one suggestion line is injected — the claim itself stays a decision
-- of the model/user (REG-3).
runHook :: IO ()
runHook = do
  outcome <- try run
  case outcome :: Either SomeException () of
    Right () -> pure ()
    Left _ -> pure ()
  where
    run = do
      raw <- BL.getContents
      case parseHookInput raw of
        Nothing -> pure ()
        Just hi -> do
          (delivered, suggestion) <- withDB $ \c -> do
            let addr = addressFromSessionId (hiSessionId hi)
            -- No pid/boot: the hook is not the serving process and
            -- must not masquerade as one.
            _ <- ensureSession c addr (hiCwd hi) Nothing Nothing
            d <- deliverPending c addr
            s <-
              if hiEventName hi == "SessionStart"
                then suggestRoleName c addr (T.unpack (hiCwd hi))
                else pure Nothing
            pure (d, s)
          for_ (hookOutput (hiEventName hi) delivered suggestion) TIO.putStr

-- | Per-event output shape: SessionStart and UserPromptSubmit add
-- plain stdout to context; every other event uses the
-- hookSpecificOutput.additionalContext envelope. Nothing pending and
-- nothing to suggest → no output at all (silence is the common case).
hookOutput :: Text -> [Delivered] -> Maybe AgentName -> Maybe Text
hookOutput event delivered suggestion
  | T.null body = Nothing
  | event `elem` ["SessionStart", "UserPromptSubmit"] = Just body
  | otherwise =
      Just . jsonToText $
        object
          [ "hookSpecificOutput"
              .= object
                [ "hookEventName" .= event
                , "additionalContext" .= body
                ]
          ]
  where
    body =
      T.concat . catMaybes $
        [ if null delivered then Nothing else Just (renderDigest (map dMessage delivered))
        , renderSuggestion <$> suggestion
        ]

renderDigest :: [Message] -> Text
renderDigest msgs =
  T.unlines $
    ("[poreus] " <> T.pack (show (length msgs)) <> " message(s) delivered:")
      : map channelDigest msgs

renderSuggestion :: AgentName -> Text
renderSuggestion (AgentName nm) =
  "[poreus] This session holds no name; the role '"
    <> nm
    <> "' is available for this workspace. If this session represents the repo, claim it with the poreus claim_name tool (ask the user when unsure).\n"
