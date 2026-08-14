module Poreus.SessionSpec (spec) where

import Test.Hspec

import Poreus.Name (boundNameOf, claimName)
import Poreus.Session
import Poreus.TestM
import Poreus.Time (formatUtc, unTimestamp)
import Poreus.Types

alice :: SessionAddress
alice = SessionAddress "s-alice"

spec :: Spec
spec = do
  describe "ensureSession (REG-1/REG-2)" $ do
    it "creates the row with first_seen = heartbeat = now and a zeroed cursor" $ do
      (row, _) <- withTestDB initialTestState $ \c ->
        ensureSession c alice "/ws/alice" (Just 500) (Just "boot-test")
      sessAddress row `shouldBe` alice
      sessWorkspace row `shouldBe` "/ws/alice"
      sessPid row `shouldBe` Just 500
      formatUtc (unTimestamp (sessFirstSeenAt row)) `shouldBe` "2026-01-01T00:00:00.000Z"
      sessHeartbeatAt row `shouldBe` sessFirstSeenAt row
      sessEndedAt row `shouldBe` Nothing

    it "refreshes heartbeat but keeps first_seen on repeat contact" $ do
      (row, _) <- withTestDB initialTestState $ \c -> do
        _ <- ensureSession c alice "/ws/alice" Nothing Nothing
        advanceClock 60
        ensureSession c alice "/ws/alice" Nothing Nothing
      formatUtc (unTimestamp (sessFirstSeenAt row)) `shouldBe` "2026-01-01T00:00:00.000Z"
      formatUtc (unTimestamp (sessHeartbeatAt row)) `shouldBe` "2026-01-01T00:01:00.000Z"

    it "revives an ended session (RECV-5: resume, same address)" $ do
      (row, _) <- withTestDB initialTestState $ \c -> do
        _ <- ensureSession c alice "/ws/alice" Nothing Nothing
        endSession c alice
        advanceClock 5
        ensureSession c alice "/ws/alice" Nothing Nothing
      sessEndedAt row `shouldBe` Nothing

    it "never lets a pid-less contact (the hook) clobber the serving pid" $ do
      (row, _) <- withTestDB initialTestState $ \c -> do
        _ <- ensureSession c alice "/ws/alice" (Just 500) (Just "boot-test")
        ensureSession c alice "/ws/alice" Nothing Nothing
      sessPid row `shouldBe` Just 500
      sessBootId row `shouldBe` Just "boot-test"

  describe "endSession" $ do
    it "stamps ended_at and releases the held name (REG-3)" $ do
      ((mrow, held), _) <- withTestDB initialTestState $ \c -> do
        _ <- ensureSession c alice "/ws/alice" Nothing Nothing
        _ <- claimName c alice "alice" False
        endSession c alice
        r <- getSession c alice
        h <- boundNameOf c alice
        pure (r, h)
      (mrow >>= sessEndedAt) `shouldSatisfy` (/= Nothing)
      held `shouldBe` Nothing

  describe "sessionLive (DISC-4 liveness)" $ do
    it "is live within the heartbeat window" $ do
      (live, _) <- withTestDB initialTestState $ \c -> do
        row <- ensureSession c alice "/ws/alice" Nothing Nothing
        advanceClock 10
        sessionLive row
      live `shouldBe` True

    it "is dead when the heartbeat is stale" $ do
      (live, _) <- withTestDB initialTestState $ \c -> do
        row <- ensureSession c alice "/ws/alice" Nothing Nothing
        advanceClock 16
        sessionLive row
      live `shouldBe` False

    it "is dead once ended, regardless of heartbeat" $ do
      (live, _) <- withTestDB initialTestState $ \c -> do
        _ <- ensureSession c alice "/ws/alice" Nothing Nothing
        endSession c alice
        row <- getSession c alice
        maybe (pure False) sessionLive row
      live `shouldBe` False

    it "fast-path dead when the serving pid is gone" $ do
      (live, _) <- withTestDB initialTestState $ \c -> do
        addProc 500 (ProcInfo Nothing "poreus" False)
        row <- ensureSession c alice "/ws/alice" (Just 500) (Just "boot-test")
        sessionLive row
      live `shouldBe` False

    it "fast-path dead when the boot id changed (host reboot)" $ do
      (live, _) <- withTestDB initialTestState $ \c -> do
        addProc 500 (ProcInfo Nothing "poreus" True)
        row <- ensureSession c alice "/ws/alice" (Just 500) (Just "boot-old")
        sessionLive row
      live `shouldBe` False

    it "live when pid corroborates and heartbeat is fresh" $ do
      (live, _) <- withTestDB initialTestState $ \c -> do
        addProc 500 (ProcInfo Nothing "poreus" True)
        row <- ensureSession c alice "/ws/alice" (Just 500) (Just "boot-test")
        sessionLive row
      live `shouldBe` True
