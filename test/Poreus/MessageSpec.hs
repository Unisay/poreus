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
    it "round-trips through text" $ do
      parseMessageKind (messageKindText MKRequest) `shouldBe` Just MKRequest
      parseMessageKind (messageKindText MKReply) `shouldBe` Just MKReply
      parseMessageKind "nope" `shouldBe` Nothing

    it "serialises as JSON string" $ do
      A.encode MKRequest `shouldBe` "\"request\""
      A.encode MKReply `shouldBe` "\"reply\""

  describe "RequestPayload" $ do
    it "round-trips through JSON" $ do
      let p =
            RequestPayload
              { rpRequestKind = KindRpc
              , rpUrl = Just "poreus://nixos/add-system-package/cowsay?flatpak=0"
              , rpDescription = Just "desc"
              , rpExpected = Nothing
              }
      A.decode (A.encode p) `shouldBe` Just p

    it "omits nothing but emits explicit null for absent fields" $ do
      let p =
            RequestPayload
              { rpRequestKind = KindFreetext
              , rpUrl = Nothing
              , rpDescription = Nothing
              , rpExpected = Nothing
              }
          s = TE.decodeUtf8 (BL.toStrict (A.encode p))
      s `shouldSatisfy` ("\"request_kind\":\"freetext\"" `T.isInfixOf`)
      s `shouldSatisfy` ("\"url\":null" `T.isInfixOf`)

  describe "ReplyPayload" $ do
    it "round-trips completed result" $ do
      let p =
            ReplyPayload
              { rplStatus = RSCompleted
              , rplSummary = Just "all good"
              , rplArtifacts = A.toJSON ([1 :: Int, 2, 3])
              , rplReason = Nothing
              }
      A.decode (A.encode p) `shouldBe` Just p

    it "round-trips rejection with reason" $ do
      let p =
            ReplyPayload
              { rplStatus = RSRejected
              , rplSummary = Just "busy"
              , rplArtifacts = A.Array mempty
              , rplReason = Just "out of capacity"
              }
      A.decode (A.encode p) `shouldBe` Just p

  describe "Message JSON shape" $ do
    it "emits message_id at the top level (not id)" $ do
      let m =
            Message
              { msgId = "20260101-000000-alice-0001"
              , msgFrom = "alice"
              , msgTo = "bob"
              , msgKind = MKRequest
              , msgInReplyTo = Nothing
              , msgPayload = A.toJSON (RequestPayload KindFreetext Nothing (Just "hi") Nothing)
              , msgCreatedAt = t0
              }
          s = TE.decodeUtf8 (BL.toStrict (A.encode m))
      s `shouldSatisfy` ("\"message_id\":\"20260101-000000-alice-0001\"" `T.isInfixOf`)
      s `shouldSatisfy` ("\"in_reply_to\":null" `T.isInfixOf`)
      s `shouldNotSatisfy` ("\"id\":" `T.isInfixOf`)

    it "reply carries in_reply_to = the original task id" $ do
      let m =
            Message
              { msgId = "20260101-000100-bob-0002"
              , msgFrom = "bob"
              , msgTo = "alice"
              , msgKind = MKReply
              , msgInReplyTo = Just "20260101-000000-alice-0001"
              , msgPayload = A.toJSON (ReplyPayload RSCompleted (Just "ok") (A.Array mempty) Nothing)
              , msgCreatedAt = t0
              }
          s = TE.decodeUtf8 (BL.toStrict (A.encode m))
      s `shouldSatisfy` ("\"in_reply_to\":\"20260101-000000-alice-0001\"" `T.isInfixOf`)
      s `shouldSatisfy` ("\"kind\":\"reply\"" `T.isInfixOf`)
