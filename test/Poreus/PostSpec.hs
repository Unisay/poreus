module Poreus.PostSpec (spec) where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as A
import Database.SQLite.Simple (Connection)
import Test.Hspec

import Poreus.Name (claimName, releaseName)
import Poreus.Post
import Poreus.Profile (EndpointInput (..), publishProfile)
import Poreus.Session (endSession, ensureSession)
import Poreus.TestM
import Poreus.Time (formatUtc, unTimestamp)
import Poreus.Types

alice, bob :: SessionAddress
alice = SessionAddress "s-alice"
bob = SessionAddress "s-bob"

fromAlice :: Sender
fromAlice = Sender alice Nothing

setup :: Connection -> TestIOM ()
setup c = do
  _ <- ensureSession c alice "/ws/alice" Nothing Nothing
  _ <- ensureSession c bob "/ws/bob" Nothing Nothing
  _ <- claimName c bob "nixos" False
  setRandomInts [0xabcd, 0x1234, 0x5678]
  pure ()

spec :: Spec
spec = do
  describe "postRequest (SEND-1, SEND-5)" $ do
    it "resolves the name at post time and stamps id, sender, timestamp" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        setup c
        postRequest c fromAlice "nixos" "deploy the thing" (Just "green CI") Nothing
      case r of
        Left e -> expectationFailure (show e)
        Right (m, ws) -> do
          ws `shouldBe` []
          msgId m `shouldBe` MessageId "20260101-000000-alice-abcd"
          msgFrom m `shouldBe` alice
          msgTo m `shouldBe` bob
          msgFromName m `shouldBe` Nothing
          msgToName m `shouldBe` Just (AgentName "nixos")
          msgKind m `shouldBe` MKRequest
          formatUtc (unTimestamp (msgCreatedAt m)) `shouldBe` "2026-01-01T00:00:00.000Z"
          msgPayload m
            `shouldBe` object
              [ "request_kind" .= ("freetext" :: String)
              , "description" .= ("deploy the thing" :: String)
              , "expected_outcome" .= ("green CI" :: String)
              ]

    it "tags the id with the sender's bound name and annotates from_name (OQ-10)" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        setup c
        _ <- claimName c alice "folios" False
        postRequest c (Sender alice (Just (AgentName "folios"))) "nixos" "hello" Nothing Nothing
      case r of
        Left e -> expectationFailure (show e)
        Right (m, _) -> do
          msgId m `shouldBe` MessageId "20260101-000000-folios-abcd"
          msgFromName m `shouldBe` Just (AgentName "folios")

    it "rejects a never-claimed name (unknown-recipient)" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        setup c
        postRequest c fromAlice "ghost" "x" Nothing Nothing
      leftCode r `shouldBe` Just UnknownRecipient

    it "rejects a claimed but unbound name (name-unbound)" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        setup c
        _ <- releaseName c bob
        postRequest c fromAlice "nixos" "x" Nothing Nothing
      leftCode r `shouldBe` Just NameUnbound

    it "accepts a direct session address without a name annotation" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        setup c
        postRequest c fromAlice "s-bob" "x" Nothing Nothing
      case r of
        Left e -> expectationFailure (show e)
        Right (m, ws) -> do
          msgTo m `shouldBe` bob
          msgToName m `shouldBe` Nothing
          ws `shouldBe` []

    it "rejects an unknown session address" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        setup c
        postRequest c fromAlice "s-ghost" "x" Nothing Nothing
      leftCode r `shouldBe` Just UnknownRecipient

    it "accepts an ended session's address with a warning (SEND-5(4))" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        setup c
        endSession c bob
        postRequest c fromAlice "s-bob" "x" Nothing Nothing
      case r of
        Left e -> expectationFailure (show e)
        Right (_, ws) -> map warnCode ws `shouldBe` ["recipient-session-ended"]

    it "rejects an empty description" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        setup c
        postRequest c fromAlice "nixos" "" Nothing Nothing
      leftCode r `shouldBe` Just InvalidInput

  describe "postCall (SEND-2)" $ do
    it "builds the rpc payload and warns when the endpoint is unknown" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        setup c
        postCall c fromAlice "nixos" "deploy-poreus" (Just (object ["sha" .= ("abc123" :: String)]))
      case r of
        Left e -> expectationFailure (show e)
        Right (m, ws) -> do
          map warnCode ws `shouldBe` ["endpoint-not-found"]
          msgPayload m
            `shouldBe` object
              [ "request_kind" .= ("rpc" :: String)
              , "verb" .= ("deploy-poreus" :: String)
              , "args" .= object ["sha" .= ("abc123" :: String)]
              ]

    it "does not warn when the endpoint is registered" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        setup c
        _ <-
          publishProfile
            c
            bob
            (Just "nixos")
            "host"
            []
            [EndpointInput "deploy-poreus" "Deploy" AutonomyConfirm Nothing]
        postCall c fromAlice "nixos" "deploy-poreus" Nothing
      fmap snd r `shouldBe` Right []

  describe "postReply (SEND-3)" $ do
    it "routes to the exact session that posted the referenced message" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        setup c
        req <- postRequest c fromAlice "nixos" "deploy" Nothing Nothing
        case req of
          Left e -> pure (Left e)
          Right (m, _) ->
            postReply c (Sender bob (Just (AgentName "nixos"))) (msgId m) "completed" (Just "done") Nothing
      case r of
        Left e -> expectationFailure (show e)
        Right (m, ws) -> do
          ws `shouldBe` []
          msgTo m `shouldBe` alice
          msgKind m `shouldBe` MKNotice
          msgInReplyTo m `shouldBe` Just (MessageId "20260101-000000-alice-abcd")
          msgPayload m
            `shouldBe` object ["event" .= ("completed" :: String), "summary" .= ("done" :: String)]

    it "rejects an unknown correlation id (unknown-message)" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        setup c
        postReply c fromAlice (MessageId "nope") "completed" Nothing Nothing
      leftCode r `shouldBe` Just UnknownMessage

    it "warns when the thread is already terminal" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        setup c
        Right (m, _) <- postRequest c fromAlice "nixos" "deploy" Nothing Nothing
        _ <- postReply c (Sender bob Nothing) (msgId m) "completed" Nothing Nothing
        postReply c (Sender bob Nothing) (msgId m) "completed" (Just "again") Nothing
      case r of
        Left e -> expectationFailure (show e)
        Right (_, ws) -> map warnCode ws `shouldBe` ["thread-already-terminal"]

    it "reaches the requester's mailbox even after it released its name (late reply)" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        setup c
        _ <- claimName c alice "folios" False
        Right (m, _) <- postRequest c (Sender alice (Just (AgentName "folios"))) "nixos" "x" Nothing Nothing
        _ <- releaseName c alice
        postReply c (Sender bob Nothing) (msgId m) "completed" Nothing Nothing
      case r of
        Left e -> expectationFailure (show e)
        Right (m, _) -> msgTo m `shouldBe` alice

  describe "postNotify (SEND-4)" $ do
    it "posts an uncorrelated notice with optional fields" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        setup c
        postNotify c fromAlice "nixos" (Just "protocol-upgraded") (Just "please re-register") Nothing
      case r of
        Left e -> expectationFailure (show e)
        Right (m, _) -> do
          msgKind m `shouldBe` MKNotice
          msgInReplyTo m `shouldBe` Nothing
          msgPayload m
            `shouldBe` object
              [ "event" .= ("protocol-upgraded" :: String)
              , "summary" .= ("please re-register" :: String)
              ]

    it "accepts a bare ping (no event, no summary)" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        setup c
        postNotify c fromAlice "s-bob" Nothing Nothing Nothing
      fmap (msgPayload . fst) r `shouldBe` Right (A.object [])

leftCode :: Either PoreusError a -> Maybe ErrorCode
leftCode = either (Just . errCode) (const Nothing)
