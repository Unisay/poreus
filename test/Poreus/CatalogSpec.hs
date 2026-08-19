module Poreus.CatalogSpec (spec) where

import Database.SQLite.Simple (Connection)
import Test.Hspec

import Poreus.Catalog
import Poreus.Name (claimName)
import Poreus.Profile (EndpointInput (..), publishProfile)
import Poreus.Session (ensureSession)
import Poreus.TestM
import Poreus.Types

alice, bob, carol :: SessionAddress
alice = SessionAddress "s-alice"
bob = SessionAddress "s-bob"
carol = SessionAddress "s-carol"

-- | alice holds "nixos" with a deploy endpoint; bob holds nothing;
-- carol's serving process is gone (dead, ADR-0017: liveness is the
-- (pid, boot_id, proc_start) triple, never a stale timestamp).
fixture :: Connection -> TestIOM ()
fixture c = do
  addProc 900 (ProcInfo Nothing "poreus" False 900)
  _ <- ensureSession c carol "/ws/carol" (Just 900) (Just "boot-test")
  -- Ordering only: discover sorts sessions by first_seen_at. Time no
  -- longer has any bearing on liveness (ADR-0017).
  advanceClock 60
  _ <- ensureSession c alice "/ws/alice" Nothing Nothing
  _ <- ensureSession c bob "/ws/bob" Nothing Nothing
  _ <-
    publishProfile
      c
      alice
      (Just "nixos")
      "NixOS host"
      ["nix"]
      [EndpointInput "deploy-poreus" "Deploy" AutonomyConfirm Nothing]
  pure ()

spec :: Spec
spec = do
  describe "discover (DISC-1)" $ do
    it "lists names with binding + liveness and all sessions (auto-provisioned included)" $ do
      (cat, _) <- withTestDB initialTestState $ \c -> do
        fixture c
        discover c noFilters
      map cnName (catNames cat) `shouldBe` [AgentName "nixos"]
      map cnBoundSession (catNames cat) `shouldBe` [Just alice]
      map cnLive (catNames cat) `shouldBe` [True]
      map csAddress (catSessions cat) `shouldBe` [carol, alice, bob]
      map csLive (catSessions cat) `shouldBe` [False, True, True]
      map csName (catSessions cat) `shouldBe` [Nothing, Just (AgentName "nixos"), Nothing]

    it "narrows to live sessions with live_only (DISC-4)" $ do
      (cat, _) <- withTestDB initialTestState $ \c -> do
        fixture c
        discover c noFilters{dfLiveOnly = True}
      map csAddress (catSessions cat) `shouldBe` [alice, bob]

    it "filters names by tag" $ do
      (cat, _) <- withTestDB initialTestState $ \c -> do
        fixture c
        discover c noFilters{dfTag = Just "nix"}
      map cnName (catNames cat) `shouldBe` [AgentName "nixos"]

    it "returns nothing for an unknown tag" $ do
      (cat, _) <- withTestDB initialTestState $ \c -> do
        fixture c
        discover c noFilters{dfTag = Just "haskell"}
      catNames cat `shouldBe` []

    it "finds providers of a verb, exact match only (DISC-2)" $ do
      ((hit, miss), _) <- withTestDB initialTestState $ \c -> do
        fixture c
        hit <- discover c noFilters{dfVerb = Just "deploy-poreus"}
        miss <- discover c noFilters{dfVerb = Just "deploy"}
        pure (hit, miss)
      map cnName (catNames hit) `shouldBe` [AgentName "nixos"]
      catNames miss `shouldBe` []

    it "restricts to one address (name or session form)" $ do
      ((byName, byAddr), _) <- withTestDB initialTestState $ \c -> do
        fixture c
        n <- discover c noFilters{dfAddress = Just "nixos"}
        a <- discover c noFilters{dfAddress = Just "s-bob"}
        pure (n, a)
      map cnName (catNames byName) `shouldBe` [AgentName "nixos"]
      map csAddress (catSessions byName) `shouldBe` [alice]
      catNames byAddr `shouldBe` []
      map csAddress (catSessions byAddr) `shouldBe` [bob]

    it "shows a released name as not live" $ do
      (cat, _) <- withTestDB initialTestState $ \c -> do
        addProc 500 (ProcInfo Nothing "poreus" True 111)
        _ <- ensureSession c alice "/ws/alice" (Just 500) (Just "boot-test")
        _ <- claimName c alice "nixos" False
        addProc 500 (ProcInfo Nothing "poreus" False 111)
        discover c noFilters
      map cnLive (catNames cat) `shouldBe` [False]
