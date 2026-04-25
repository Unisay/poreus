module Poreus.MessageSpec (spec) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (defaultTimeLocale, parseTimeOrError)
import Test.Hspec

import Poreus.Message
import Poreus.Time (Timestamp (..))
import Poreus.Types

t0 :: Timestamp
t0 =
  Timestamp
    ( parseTimeOrError
        True
        defaultTimeLocale
        "%Y-%m-%dT%H:%M:%S%Z"
        "2026-01-01T00:00:00Z"
    )

spec :: Spec
spec = do
  describe "MessageKind" $ do
    it "round-trips canonical names" $ do
      parseMessageKind (messageKindText MKRequest) `shouldBe` Just MKRequest
      parseMessageKind (messageKindText MKNotice) `shouldBe` Just MKNotice
      parseMessageKind "nope" `shouldBe` Nothing

    it "accepts legacy 'reply' as MKNotice (transition tolerance)" $
      parseMessageKind "reply" `shouldBe` Just MKNotice

    it "serialises as canonical JSON string" $ do
      A.encode MKRequest `shouldBe` "\"request\""
      A.encode MKNotice `shouldBe` "\"notice\""

  describe "Message JSON shape" $ do
    it "emits message_id at the top level (not id)" $ do
      let m =
            Message
              { msgId = "20260101-000000-alice-0001"
              , msgFrom = "alice"
              , msgTo = "bob"
              , msgKind = MKRequest
              , msgInReplyTo = Nothing
              , msgPayload = A.object ["description" A..= ("hi" :: T.Text)]
              , msgSubscribe = Nothing
              , msgCreatedAt = t0
              }
          s = TE.decodeUtf8 (BL.toStrict (A.encode m))
      s `shouldSatisfy` ("\"message_id\":\"20260101-000000-alice-0001\"" `T.isInfixOf`)
      s `shouldSatisfy` ("\"in_reply_to\":null" `T.isInfixOf`)
      s `shouldSatisfy` ("\"subscribe\":null" `T.isInfixOf`)
      s `shouldNotSatisfy` ("\"id\":" `T.isInfixOf`)

    it "notice carries in_reply_to = the original message id" $ do
      let m =
            Message
              { msgId = "20260101-000100-bob-0002"
              , msgFrom = "bob"
              , msgTo = "alice"
              , msgKind = MKNotice
              , msgInReplyTo = Just "20260101-000000-alice-0001"
              , msgPayload = A.object ["event" A..= ("completed" :: T.Text)]
              , msgSubscribe = Nothing
              , msgCreatedAt = t0
              }
          s = TE.decodeUtf8 (BL.toStrict (A.encode m))
      s `shouldSatisfy` ("\"in_reply_to\":\"20260101-000000-alice-0001\"" `T.isInfixOf`)
      s `shouldSatisfy` ("\"kind\":\"notice\"" `T.isInfixOf`)

    it "request with subscribe round-trips through DB columns" $ do
      let m =
            Message
              { msgId = "rid"
              , msgFrom = "alice"
              , msgTo = "bob"
              , msgKind = MKRequest
              , msgInReplyTo = Nothing
              , msgPayload = A.object ["description" A..= ("x" :: T.Text)]
              , msgSubscribe = Just ["started", "completed", "failed"]
              , msgCreatedAt = t0
              }
          s = TE.decodeUtf8 (BL.toStrict (A.encode m))
      s `shouldSatisfy` ("\"subscribe\":[\"started\",\"completed\",\"failed\"]" `T.isInfixOf`)
