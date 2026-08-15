module Poreus.HookSpec (spec) where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as A
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, parseTimeOrError)
import Test.Hspec

import Poreus.Deliver (Delivered (..), replyDuty)
import Poreus.Hook
import Poreus.Time (Timestamp (..))
import Poreus.Types

sampleRequest :: Message
sampleRequest =
  Message
    { msgSeq = 1
    , msgId = MessageId "20260101-000000-alice-abcd"
    , msgFrom = SessionAddress "s-alice"
    , msgTo = SessionAddress "s-bob"
    , msgFromName = Nothing
    , msgToName = Just (AgentName "nixos")
    , msgKind = MKRequest
    , msgInReplyTo = Nothing
    , msgPayload = object ["request_kind" .= ("freetext" :: String), "description" .= ("deploy" :: String)]
    , msgCreatedAt = Timestamp (parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Z" "2026-01-01T00:00:00Z")
    }

delivered :: Delivered
delivered = Delivered sampleRequest (Just (replyDuty (msgId sampleRequest)))

spec :: Spec
spec = do
  describe "parseHookInput" $ do
    it "reads session_id, cwd, and event name" $ do
      let raw = A.encode (object ["session_id" .= ("abc" :: String), "cwd" .= ("/ws" :: String), "hook_event_name" .= ("UserPromptSubmit" :: String)])
      parseHookInput raw `shouldBe` Just (HookInput "abc" "/ws" "UserPromptSubmit")

    it "requires session_id" $ do
      parseHookInput (A.encode (object ["cwd" .= ("/ws" :: String)])) `shouldBe` Nothing
      parseHookInput "garbage" `shouldBe` Nothing

  describe "hookOutput" $ do
    it "is silent when nothing is pending and there is nothing to suggest" $ do
      hookOutput "UserPromptSubmit" [] Nothing `shouldBe` Nothing
      hookOutput "SessionStart" [] Nothing `shouldBe` Nothing

    it "emits a plain context digest for SessionStart and UserPromptSubmit" $ do
      case hookOutput "UserPromptSubmit" [delivered] Nothing of
        Just out -> do
          out `shouldSatisfy` T.isInfixOf "[poreus] 1 message(s) delivered"
          out `shouldSatisfy` T.isInfixOf "20260101-000000-alice-abcd"
          out `shouldSatisfy` T.isInfixOf "reply"
        Nothing -> expectationFailure "expected output"

    it "wraps other events in hookSpecificOutput.additionalContext" $ do
      case hookOutput "PostToolUse" [delivered] Nothing of
        Just out -> do
          out `shouldSatisfy` T.isInfixOf "hookSpecificOutput"
          out `shouldSatisfy` T.isInfixOf "PostToolUse"
          out `shouldSatisfy` T.isInfixOf "additionalContext"
        Nothing -> expectationFailure "expected output"

    it "surfaces the role suggestion alone on SessionStart" $ do
      case hookOutput "SessionStart" [] (Just (AgentName "poreus")) of
        Just out -> do
          out `shouldSatisfy` T.isInfixOf "role 'poreus' is available"
          out `shouldSatisfy` T.isInfixOf "claim_name"
        Nothing -> expectationFailure "expected output"

    it "combines pending messages with the role suggestion" $ do
      case hookOutput "SessionStart" [delivered] (Just (AgentName "poreus")) of
        Just out -> do
          out `shouldSatisfy` T.isInfixOf "1 message(s) delivered"
          out `shouldSatisfy` T.isInfixOf "claim_name"
        Nothing -> expectationFailure "expected output"
