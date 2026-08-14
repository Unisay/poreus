module Poreus.Hook
  ( runHook

    -- * Pure pieces exposed for tests
  , HookInput (..)
  , parseHookInput
  , renderDigest
  , hookOutput
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (for_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Poreus.DB (withDB)
import Poreus.Deliver (Delivered (..), deliverPending)
import Poreus.Identity (addressFromSessionId)
import Poreus.JSON (jsonToText)
import Poreus.Mcp.Channel (channelDigest)
import Poreus.Session (ensureSession)
import Poreus.Types (Message)

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
          delivered <- withDB $ \c -> do
            let addr = addressFromSessionId (hiSessionId hi)
            -- No pid/boot: the hook is not the serving process and
            -- must not masquerade as one.
            _ <- ensureSession c addr (hiCwd hi) Nothing Nothing
            deliverPending c addr
          for_ (hookOutput (hiEventName hi) delivered) TIO.putStr

-- | Per-event output shape: SessionStart and UserPromptSubmit add
-- plain stdout to context; every other event uses the
-- hookSpecificOutput.additionalContext envelope. Nothing pending →
-- no output at all (silence is the common case).
hookOutput :: Text -> [Delivered] -> Maybe Text
hookOutput _ [] = Nothing
hookOutput event delivered
  | event `elem` ["SessionStart", "UserPromptSubmit"] = Just digest
  | otherwise =
      Just . jsonToText $
        object
          [ "hookSpecificOutput"
              .= object
                [ "hookEventName" .= event
                , "additionalContext" .= digest
                ]
          ]
  where
    digest = renderDigest (map dMessage delivered)

renderDigest :: [Message] -> Text
renderDigest msgs =
  T.unlines $
    ("[poreus] " <> T.pack (show (length msgs)) <> " message(s) delivered:")
      : map channelDigest msgs
