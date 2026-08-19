{-# LANGUAGE FlexibleContexts #-}

module Poreus.DoorbellSpec (spec) where

import qualified Control.Monad.State.Strict as MS
import Data.Text (Text)
import Database.SQLite.Simple (Connection)
import Test.Hspec

import Poreus.Doorbell
import Poreus.Identity (Identity (..), resolveIdentityFrom)
import Poreus.Name (claimName, releaseName)
import Poreus.Session (endSession, ensureSession)
import Poreus.TestM
import Poreus.Types

bob :: SessionAddress
bob = SessionAddress "s-bob"

nixos :: Mailbox
nixos = MailboxRole (AgentName "nixos")

-- | A claude host process at pid 200 that publishes a name for the
-- session poreus is about to record.
claudeHost :: MS.MonadState TestState m => Text -> m ()
claudeHost name = do
  setMyPid 100
  addProc 100 (ProcInfo (Just 200) "poreus" True 10)
  addProc 200 (ProcInfo Nothing "claude" True 20)
  addProc 500 (ProcInfo Nothing "poreus" True 111)
  setEnv "CLAUDE_CONFIG_DIR" "/cfg"
  addFile "/cfg/sessions/200.json" ("{\"pid\":200,\"name\":\"" <> name <> "\"}")

holderIsLive :: Connection -> TestIOM ()
holderIsLive c = do
  claudeHost "nixos-window"
  -- Seed the identity map the way a real server start does, so the
  -- session can be joined back to its claude process.
  addr <- idAddress <$> resolveIdentityFrom c (Just "bob") "/ws/bob"
  _ <- ensureSession c addr "/ws/bob" (Just 500) (Just "boot-test")
  _ <- claimName c addr "nixos" False
  pure ()

spec :: Spec
spec = do
  describe "doorbellFor" $ do
    it "names the host session exactly, never a workspace guess" $ do
      -- Two live sessions shared one repository on 2026-08-18 and a
      -- workspace match picked the wrong one. A latency layer must not
      -- be able to reintroduce that (ADR-0017, L6).
      (bell, _) <- withTestDB initialTestState $ \c -> do
        holderIsLive c
        doorbellFor c nixos
      fmap dbAgent bell `shouldBe` Just "nixos-window"

    it "carries the fixed payload-free body" $ do
      (bell, _) <- withTestDB initialTestState $ \c -> do
        holderIsLive c
        doorbellFor c nixos
      fmap dbBody bell `shouldBe` Just doorbellBody

    it "works the same for a session mailbox" $ do
      (bell, _) <- withTestDB initialTestState $ \c -> do
        holderIsLive c
        doorbellFor c (MailboxSession bob)
      fmap dbAgent bell `shouldBe` Just "nixos-window"

    it "is silent when no session holds the role" $ do
      (bell, _) <- withTestDB initialTestState $ \c -> do
        holderIsLive c
        _ <- releaseName c bob
        doorbellFor c nixos
      bell `shouldBe` Nothing

    it "is silent when the holder's process is gone" $ do
      (bell, _) <- withTestDB initialTestState $ \c -> do
        holderIsLive c
        addProc 500 (ProcInfo Nothing "poreus" False 111)
        doorbellFor c nixos
      bell `shouldBe` Nothing

    it "is silent when the holder ended cleanly" $ do
      (bell, _) <- withTestDB initialTestState $ \c -> do
        holderIsLive c
        endSession c bob
        doorbellFor c nixos
      bell `shouldBe` Nothing

    it "rings the host's CURRENT name, not the stored lease" $ do
      -- The lease is renewed when a session is ACTIVE; the doorbell
      -- exists to reach one that is IDLE. So the lease is least
      -- trustworthy in exactly the state the doorbell is used in.
      -- Measured 2026-08-19: both stale-lease sessions on this host
      -- were idle, and both were the ones worth ringing.
      (bell, _) <- withTestDB initialTestState $ \c -> do
        holderIsLive c
        -- The user renames the session; it stays idle, so no hook runs
        -- and the lease is never renewed.
        addFile "/cfg/sessions/200.json" "{\"pid\":200,\"name\":\"renamed-window\"}"
        doorbellFor c nixos
      fmap dbAgent bell `shouldBe` Just "renamed-window"

    it "is silent when the host published no name for the holder" $ do
      -- Nothing to ring is not an error: the message is in the ledger
      -- and arrives at the recipient's next prompt or tool call.
      (bell, _) <- withTestDB initialTestState $ \c -> do
        addProc 500 (ProcInfo Nothing "poreus" True 111)
        _ <- ensureSession c bob "/ws/bob" (Just 500) (Just "boot-test")
        _ <- claimName c bob "nixos" False
        doorbellFor c nixos
      bell `shouldBe` Nothing

    it "is silent for a role that does not exist" $ do
      (bell, _) <- withTestDB initialTestState $ \c ->
        doorbellFor c (MailboxRole (AgentName "ghost"))
      bell `shouldBe` Nothing
