module Poreus.ProfileSpec (spec) where

import qualified Data.Aeson as A
import Data.Text (Text)
import Test.Hspec

import Poreus.Endpoint (loadAgent)
import Poreus.Profile
import Poreus.Time (Timestamp (..))
import Poreus.TestM
import Poreus.Types

import Data.Time (addUTCTime)

spec :: Spec
spec = do
  describe "registerAgent" $ do
    it "inserts a new row and returns (now, now)" $ do
      let t0 = tsClock emptyTestState
      (r, _) <- withMemoryDB emptyTestState $ \c -> do
        registerAgent c "foo" "/tmp/foo" (Timestamp t0)
      r `shouldBe` (Timestamp t0, Timestamp t0)

    it "preserves registered_at on re-register but bumps updated_at" $ do
      let t0 = tsClock emptyTestState
          t1 = addUTCTime 60 t0
      ((r1, r2), _) <- withMemoryDB emptyTestState $ \c -> do
        reg1 <- registerAgent c "foo" "/tmp/foo" (Timestamp t0)
        reg2 <- registerAgent c "foo" "/tmp/foo-new" (Timestamp t1)
        pure (reg1, reg2)
      fst r1 `shouldBe` Timestamp t0
      fst r2 `shouldBe` Timestamp t0
      snd r2 `shouldBe` Timestamp t1

  describe "putProfile" $ do
    it "stores summary, tags and endpoints; bumps updated_at" $ do
      let t0 = tsClock emptyTestState
          t1 = addUTCTime 60 t0
          profile =
            ProfileInput
              { piSummary = Just "a short summary"
              , piTags = ["nix", "infra"]
              , piEndpoints =
                  [ EndpointInput
                      { eiVerb = "ping"
                      , eiArgSchema = Nothing
                      , eiParamSchema = Nothing
                      , eiAutonomy = AutonomyAuto
                      , eiDescription = Just "ping"
                      }
                  , EndpointInput
                      { eiVerb = "deploy"
                      , eiArgSchema = Just (A.String "arg-schema")
                      , eiParamSchema = Just (A.object [])
                      , eiAutonomy = AutonomyConfirm
                      , eiDescription = Nothing
                      }
                  ]
              }
      (out, _) <- withMemoryDB emptyTestState $ \c -> do
        _ <- registerAgent c "foo" "/tmp/foo" (Timestamp t0)
        (n, ts) <- putProfile c "foo" profile (Timestamp t1)
        agent <- loadAgent c "foo"
        pure (n, ts, agent)
      case out of
        (n, ts, Just a) -> do
          n `shouldBe` 2
          ts `shouldBe` Timestamp t1
          agentSummary a `shouldBe` Just "a short summary"
          agentTags a `shouldMatchList` ["nix", "infra"]
          agentUpdatedAt a `shouldBe` Timestamp t1
          map endpointVerb (agentEndpoints a)
            `shouldMatchList` ["ping", "deploy"]
        _ -> expectationFailure "expected profile + loaded agent"

    it "replaces the endpoint set on each call" $ do
      let t0 = tsClock emptyTestState
          profile vs =
            ProfileInput
              { piSummary = Nothing
              , piTags = []
              , piEndpoints =
                  [ EndpointInput
                      { eiVerb = v
                      , eiArgSchema = Nothing
                      , eiParamSchema = Nothing
                      , eiAutonomy = AutonomyAuto
                      , eiDescription = Nothing
                      }
                  | v <- vs
                  ]
              }
      (Just a, _) <- withMemoryDB emptyTestState $ \c -> do
        _ <- registerAgent c "foo" "/tmp/foo" (Timestamp t0)
        _ <- putProfile c "foo" (profile ["a", "b", "c"]) (Timestamp t0)
        _ <- putProfile c "foo" (profile ["z"]) (Timestamp t0)
        loadAgent c "foo"
      map endpointVerb (agentEndpoints a) `shouldBe` ["z"]

  describe "jsonToText / textToJson" $ do
    it "round-trip a structured JSON value" $ do
      let v =
            A.object
              [ "k" A..= (42 :: Int)
              , "xs" A..= (["a", "b"] :: [Text])
              ]
      textToJson (jsonToText v) `shouldBe` Just v
