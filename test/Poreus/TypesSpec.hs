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

    it "rejects unknown tokens" $
      parseAutonomy "yolo" `shouldBe` Nothing

    it "round-trips through JSON" $ do
      A.decode (A.encode AutonomyAuto) `shouldBe` Just AutonomyAuto
      A.decode (A.encode AutonomyConfirm) `shouldBe` Just AutonomyConfirm
