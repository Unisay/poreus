module Poreus.CatalogSpec (spec) where

import Database.SQLite.Simple (Connection)
import Test.Hspec

import Poreus.Catalog
import Poreus.Identity (Identity (..), resolveIdentityFrom)
import Poreus.Name (claimName, releaseName)
import Poreus.Post (Sender (..), postRequest)
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
    it "lists roles with holder presence and all sessions (auto-provisioned included)" $ do
      (cat, _) <- withTestDB initialTestState $ \c -> do
        fixture c
        discover c noFilters
      map cnName (catNames cat) `shouldBe` [AgentName "nixos"]
      map cnHolderProcess (catNames cat) `shouldBe` [Just "alive"]
      map cnQueued (catNames cat) `shouldBe` [0]
      map csAddress (catSessions cat) `shouldBe` [carol, alice, bob]
      map csProcess (catSessions cat) `shouldBe` ["dead", "alive", "alive"]
      map csName (catSessions cat) `shouldBe` [Nothing, Just (AgentName "nixos"), Nothing]

    it "reports a role nobody holds as holder_process null, not as absent" $ do
      -- The 2026-08-18 misroute: live_only returned an empty list, the
      -- caller read it as "no such role", and guessed a session by
      -- workspace instead. Presence annotates; it never filters
      -- (ADR-0017 §6).
      (cat, _) <- withTestDB initialTestState $ \c -> do
        fixture c
        _ <- releaseName c alice
        discover c noFilters
      map cnName (catNames cat) `shouldBe` [AgentName "nixos"]
      map cnHolderProcess (catNames cat) `shouldBe` [Nothing]

    it "counts a role's undelivered backlog" $ do
      (cat, _) <- withTestDB initialTestState $ \c -> do
        fixture c
        setRandomInts [0 ..]
        _ <- postRequest c (Sender bob Nothing) "nixos" "work" Nothing Nothing False
        discover c noFilters
      map cnQueued (catNames cat) `shouldBe` [1]

    it "advertises the host's CURRENT name as the ring target" $ do
      -- `holder_host_name` is what a peer reads and rings. A stored
      -- copy is renewed when the session is ACTIVE, so it is least
      -- trustworthy for the idle session a ring is for. Measured
      -- 2026-08-19: the catalog advertised 'claude-config-5d' for a
      -- live role the host had called 'debug-poreus' for hours.
      (cat, _) <- withTestDB initialTestState $ \c -> do
        setMyPid 100
        addProc 100 (ProcInfo (Just 200) "poreus" True 10)
        addProc 200 (ProcInfo Nothing "claude" True 20)
        addProc 500 (ProcInfo (Just 200) "poreus" True 111)
        setEnv "CLAUDE_CONFIG_DIR" "/cfg"
        addFile "/cfg/sessions/200.json" "{\"pid\":200,\"name\":\"nixos-65\"}"
        addr <- idAddress <$> resolveIdentityFrom c (Just "holder") "/ws/holder"
        _ <- ensureSession c addr "/ws/holder" (Just 500) (Just "boot-test")
        _ <- claimName c addr "nixos" False
        -- Renamed, and idle ever since.
        addFile "/cfg/sessions/200.json" "{\"pid\":200,\"name\":\"kairos-hermes\"}"
        discover c noFilters
      map cnHolderHostName (catNames cat) `shouldBe` [Just "kairos-hermes"]

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

    it "shows a role whose holder process died as holder_process dead" $ do
      (cat, _) <- withTestDB initialTestState $ \c -> do
        addProc 500 (ProcInfo Nothing "poreus" True 111)
        _ <- ensureSession c alice "/ws/alice" (Just 500) (Just "boot-test")
        _ <- claimName c alice "nixos" False
        addProc 500 (ProcInfo Nothing "poreus" False 111)
        discover c noFilters
      map cnHolderProcess (catNames cat) `shouldBe` [Just "dead"]
