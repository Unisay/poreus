module Poreus.ConfigSpec (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Poreus.Config
import Poreus.TestM

spec :: Spec
spec = do
  describe "poreusHome resolution" $ do
    it "prefers $POREUS_HOME when set" $ do
      let st = execTestM (setEnv "POREUS_HOME" "/custom/poreus") emptyTestState
      evalTestM poreusHome st `shouldBe` "/custom/poreus"

    it "falls back to $XDG_DATA_HOME/poreus" $ do
      let st = execTestM (setEnv "XDG_DATA_HOME" "/x/share") emptyTestState
      evalTestM poreusHome st `shouldBe` "/x/share/poreus"

    it "falls back to HOME/.local/share/poreus when no env is set" $ do
      evalTestM poreusHome emptyTestState
        `shouldBe` "/home/test/.local/share/poreus"

    it "ignores empty $POREUS_HOME string" $ do
      let st = execTestM (setEnv "POREUS_HOME" "") emptyTestState
      evalTestM poreusHome st
        `shouldBe` "/home/test/.local/share/poreus"

  describe "dbPath" $ do
    it "appends db.sqlite" $ do
      let st = execTestM (setEnv "POREUS_HOME" "/p") emptyTestState
      evalTestM dbPath st `shouldBe` "/p/db.sqlite"

  describe "ensureHome" $ do
    it "creates the home directory and returns its path" $ do
      let st = execTestM (setEnv "POREUS_HOME" "/p") emptyTestState
          (home, final) = runTestM ensureHome st
      home `shouldBe` "/p"
      Map.member "/p" (tsDirs final) `shouldBe` True
