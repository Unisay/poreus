module Poreus.TypesSpec (spec) where

import qualified Data.Aeson as A
import Test.Hspec

import Poreus.Types

spec :: Spec
spec = do
  describe "Autonomy" $ do
    it "round-trips through text" $ do
      (autonomyText AutonomyAuto, parseAutonomy "auto") `shouldBe` ("auto", Just AutonomyAuto)
      (autonomyText AutonomyConfirm, parseAutonomy "confirm")
        `shouldBe` ("confirm", Just AutonomyConfirm)

    it "rejects unknown tokens" $ do
      parseAutonomy "yolo" `shouldBe` Nothing

    it "round-trips through JSON" $ do
      A.decode (A.encode AutonomyAuto) `shouldBe` Just AutonomyAuto
      A.decode (A.encode AutonomyConfirm) `shouldBe` Just AutonomyConfirm

  describe "TaskKind" $ do
    it "round-trips through text" $ do
      parseTaskKind "freetext" `shouldBe` Just KindFreetext
      parseTaskKind "rpc" `shouldBe` Just KindRpc
      parseTaskKind "nope" `shouldBe` Nothing

  describe "TaskStatus" $ do
    it "renders a stable set of tokens" $ do
      map taskStatusText
        [TSPending, TSClaimed, TSCompleted, TSFailed, TSRejected]
        `shouldBe` ["pending", "claimed", "completed", "failed", "rejected"]

    it "round-trips all known constructors" $
      mapM_
        (\s -> parseTaskStatus (taskStatusText s) `shouldBe` Just s)
        [TSPending, TSClaimed, TSCompleted, TSFailed, TSRejected]

  describe "ResultStatus" $ do
    it "round-trips all known constructors" $
      mapM_
        (\s -> parseResultStatus (resultStatusText s) `shouldBe` Just s)
        [RSCompleted, RSFailed, RSRejected]

  describe "validateTransition (state machine)" $ do
    it "allows claim on pending only" $ do
      validateTransition TSPending TrClaim `shouldBe` Right TSClaimed
      validateTransition TSClaimed TrClaim
        `shouldBe` Left (TransitionForbidden TSClaimed TrClaim)
      validateTransition TSCompleted TrClaim
        `shouldBe` Left (TransitionForbidden TSCompleted TrClaim)
      validateTransition TSFailed TrClaim
        `shouldBe` Left (TransitionForbidden TSFailed TrClaim)
      validateTransition TSRejected TrClaim
        `shouldBe` Left (TransitionForbidden TSRejected TrClaim)

    it "allows complete/fail on claimed only" $ do
      validateTransition TSClaimed TrComplete `shouldBe` Right TSCompleted
      validateTransition TSClaimed TrFail `shouldBe` Right TSFailed
      validateTransition TSPending TrComplete
        `shouldBe` Left (TransitionForbidden TSPending TrComplete)
      validateTransition TSCompleted TrComplete
        `shouldBe` Left (TransitionForbidden TSCompleted TrComplete)

    it "allows reject from pending or claimed" $ do
      validateTransition TSPending TrReject `shouldBe` Right TSRejected
      validateTransition TSClaimed TrReject `shouldBe` Right TSRejected
      validateTransition TSCompleted TrReject
        `shouldBe` Left (TransitionForbidden TSCompleted TrReject)
      validateTransition TSRejected TrReject
        `shouldBe` Left (TransitionForbidden TSRejected TrReject)
