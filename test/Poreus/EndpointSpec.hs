module Poreus.EndpointSpec (spec) where

import Data.Text (Text)
import Test.Hspec

import Poreus.Endpoint
import Poreus.Profile
import Poreus.TestM
import Poreus.Time (Timestamp (..))
import Poreus.Types

-- | Convenience endpoint constructor.
ep :: Autonomy -> Text -> EndpointInput
ep autonomy verb =
  EndpointInput
    { eiVerb = verb
    , eiArgSchema = Nothing
    , eiParamSchema = Nothing
    , eiAutonomy = autonomy
    , eiDescription = Nothing
    }

-- | Convenience profile constructor.
profile :: Autonomy -> [Text] -> [Text] -> ProfileInput
profile autonomy tags verbs =
  ProfileInput
    { piSummary = Nothing
    , piTags = tags
    , piEndpoints = map (ep autonomy) verbs
    }

spec :: Spec
spec = do
  describe "loadAllAgents + loadAgent" $ do
    it "returns registered agents in alias order" $ do
      let t0 = tsClock emptyTestState
      (agents, _) <- withMemoryDB emptyTestState $ \c -> do
        mapM_
          (\(a, p) -> registerAgent c a p (Timestamp t0))
          [("bravo", "/b"), ("alpha", "/a"), ("charlie", "/c")]
        loadAllAgents c
      map agentAlias agents `shouldBe` ["alpha", "bravo", "charlie"]

    it "loadAgent returns Nothing for unknown aliases" $ do
      (r, _) <- withMemoryDB emptyTestState $ \c -> loadAgent c "ghost"
      r `shouldBe` Nothing

  describe "agentsMatchingVerb" $ do
    it "returns agents exposing the exact verb" $ do
      let t0 = tsClock emptyTestState
      (aliases, _) <- withMemoryDB emptyTestState $ \c -> do
        mapM_
          (\a -> registerAgent c a ("/" <> unAlias a) (Timestamp t0))
          ["alpha", "bravo", "charlie"]
        _ <- putProfile c "alpha" (profile AutonomyAuto [] ["ping", "deploy"]) (Timestamp t0)
        _ <- putProfile c "bravo" (profile AutonomyAuto [] ["ping"]) (Timestamp t0)
        _ <- putProfile c "charlie" (profile AutonomyAuto [] ["status"]) (Timestamp t0)
        agents <- agentsMatchingVerb c "ping"
        pure (map agentAlias agents)
      aliases `shouldMatchList` ["alpha", "bravo"]

  describe "agentsMatchingTag" $ do
    it "filters by tag membership" $ do
      let t0 = tsClock emptyTestState
      (aliases, _) <- withMemoryDB emptyTestState $ \c -> do
        mapM_
          (\a -> registerAgent c a ("/" <> unAlias a) (Timestamp t0))
          ["alpha", "bravo"]
        _ <- putProfile c "alpha" (profile AutonomyAuto ["infra", "nix"] []) (Timestamp t0)
        _ <- putProfile c "bravo" (profile AutonomyAuto ["ui"] []) (Timestamp t0)
        agents <- agentsMatchingTag c "nix"
        pure (map agentAlias agents)
      aliases `shouldBe` ["alpha"]

  describe "matchEndpoints (pure filter)" $ do
    it "returns only matching endpoints for a loaded agent" $ do
      let t0 = tsClock emptyTestState
          prof =
            ProfileInput
              { piSummary = Nothing
              , piTags = []
              , piEndpoints = [ep AutonomyAuto "ping", ep AutonomyConfirm "deploy"]
              }
      (verbs, _) <- withMemoryDB emptyTestState $ \c -> do
        _ <- registerAgent c "alpha" "/a" (Timestamp t0)
        _ <- putProfile c "alpha" prof (Timestamp t0)
        Just a <- loadAgent c "alpha"
        pure (map endpointVerb (matchEndpoints a "ping"))
      verbs `shouldBe` ["ping"]
