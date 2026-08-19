{-# LANGUAGE FlexibleContexts #-}

module Poreus.SessionSpec (spec) where

import qualified Control.Monad.State.Strict as MS
import Data.Text (Text)
import Test.Hspec

import Poreus.Name (boundNameOf, claimName)
import Poreus.Session
import Poreus.TestM
import Poreus.Time (formatUtc, unTimestamp)
import Poreus.Types

alice :: SessionAddress
alice = SessionAddress "s-alice"

-- | A scripted claude host process at pid 200, parent of this one,
-- publishing a session file under $CLAUDE_CONFIG_DIR.
claudeHost :: MS.MonadState TestState m => Text -> m ()
claudeHost name = do
  setMyPid 100
  addProc 100 (ProcInfo (Just 200) "poreus" True 10)
  addProc 200 (ProcInfo Nothing "claude" True 20)
  setEnv "CLAUDE_CONFIG_DIR" "/cfg"
  addFile
    "/cfg/sessions/200.json"
    ( "{\"pid\":200,\"sessionId\":\"abc\",\"cwd\":\"/ws/alice\",\"procStart\":\"20\"\
      \,\"status\":\"idle\",\"statusUpdatedAt\":1787081924146,\"name\":\""
        <> name
        <> "\"}"
    )

spec :: Spec
spec = do
  describe "ensureSession (REG-1/REG-2)" $ do
    it "creates the row with first_seen = last_seen = now and a zeroed cursor" $ do
      (row, _) <- withTestDB initialTestState $ \c ->
        ensureSession c alice "/ws/alice" (Just 500) (Just "boot-test")
      sessAddress row `shouldBe` alice
      sessWorkspace row `shouldBe` "/ws/alice"
      sessPid row `shouldBe` Just 500
      formatUtc (unTimestamp (sessFirstSeenAt row)) `shouldBe` "2026-01-01T00:00:00.000Z"
      sessLastSeenAt row `shouldBe` sessFirstSeenAt row
      sessEndedAt row `shouldBe` Nothing

    it "refreshes last_seen but keeps first_seen on repeat contact" $ do
      (row, _) <- withTestDB initialTestState $ \c -> do
        _ <- ensureSession c alice "/ws/alice" Nothing Nothing
        advanceClock 60
        ensureSession c alice "/ws/alice" Nothing Nothing
      formatUtc (unTimestamp (sessFirstSeenAt row)) `shouldBe` "2026-01-01T00:00:00.000Z"
      formatUtc (unTimestamp (sessLastSeenAt row)) `shouldBe` "2026-01-01T00:01:00.000Z"

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

  describe "the host-name lease (ADR-0017 §5)" $ do
    it "reads the host's name for this session from its session file" $ do
      (row, _) <- withTestDB initialTestState $ \c -> do
        claudeHost "redesign"
        ensureSession c alice "/ws/alice" (Just 500) (Just "boot-test")
      sessHostName row `shouldBe` Just "redesign"

    it "renews on every contact, so a mid-session rename propagates" $ do
      -- This is the whole reason it is a lease. The host renamed this
      -- design's own session while the design was under review; a name
      -- captured once would have kept ringing the old one.
      (row, _) <- withTestDB initialTestState $ \c -> do
        claudeHost "poreus-transport"
        _ <- ensureSession c alice "/ws/alice" (Just 500) (Just "boot-test")
        claudeHost "redesign"
        ensureSession c alice "/ws/alice" (Just 500) (Just "boot-test")
      sessHostName row `shouldBe` Just "redesign"

    it "keeps the last known name when the file is unreadable" $ do
      (row, _) <- withTestDB initialTestState $ \c -> do
        claudeHost "redesign"
        _ <- ensureSession c alice "/ws/alice" (Just 500) (Just "boot-test")
        unsetEnv "CLAUDE_CONFIG_DIR"
        ensureSession c alice "/ws/alice" (Just 500) (Just "boot-test")
      sessHostName row `shouldBe` Just "redesign"

    it "is Nothing when no claude process is an ancestor" $ do
      (row, _) <- withTestDB initialTestState $ \c ->
        ensureSession c alice "/ws/alice" (Just 500) (Just "boot-test")
      sessHostName row `shouldBe` Nothing

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
    -- ADR-0017: liveness is the (pid, boot_id, proc_start) triple read
    -- against the OS. No stored timestamp participates, so the passage
    -- of time alone can neither kill nor revive a session.
    it "is live when no serving process has identified itself yet" $ do
      -- The hook creates the row before the server answers for it; see
      -- the Note on sessionLive for why that is not a false alive.
      (live, _) <- withTestDB initialTestState $ \c -> do
        row <- ensureSession c alice "/ws/alice" Nothing Nothing
        sessionLive row
      live `shouldBe` True

    it "stays live however long the session sits idle" $ do
      (live, _) <- withTestDB initialTestState $ \c -> do
        addProc 500 (ProcInfo Nothing "poreus" True 111)
        row <- ensureSession c alice "/ws/alice" (Just 500) (Just "boot-test")
        advanceClock 86400
        sessionLive row
      live `shouldBe` True

    it "is dead when the pid was recycled by another process" $ do
      (live, _) <- withTestDB initialTestState $ \c -> do
        addProc 500 (ProcInfo Nothing "poreus" True 111)
        row <- ensureSession c alice "/ws/alice" (Just 500) (Just "boot-test")
        -- Same pid, different process instance.
        addProc 500 (ProcInfo Nothing "something-else" True 999)
        sessionLive row
      live `shouldBe` False

    it "is dead once ended, whatever the process is doing" $ do
      (live, _) <- withTestDB initialTestState $ \c -> do
        _ <- ensureSession c alice "/ws/alice" Nothing Nothing
        endSession c alice
        row <- getSession c alice
        maybe (pure False) sessionLive row
      live `shouldBe` False

    it "fast-path dead when the serving pid is gone" $ do
      (live, _) <- withTestDB initialTestState $ \c -> do
        addProc 500 (ProcInfo Nothing "poreus" False 111)
        row <- ensureSession c alice "/ws/alice" (Just 500) (Just "boot-test")
        sessionLive row
      live `shouldBe` False

    it "fast-path dead when the boot id changed (host reboot)" $ do
      (live, _) <- withTestDB initialTestState $ \c -> do
        addProc 500 (ProcInfo Nothing "poreus" True 111)
        row <- ensureSession c alice "/ws/alice" (Just 500) (Just "boot-old")
        sessionLive row
      live `shouldBe` False

    it "live when the whole triple corroborates" $ do
      (live, _) <- withTestDB initialTestState $ \c -> do
        addProc 500 (ProcInfo Nothing "poreus" True 111)
        row <- ensureSession c alice "/ws/alice" (Just 500) (Just "boot-test")
        sessionLive row
      live `shouldBe` True
