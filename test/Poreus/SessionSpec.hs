{-# LANGUAGE FlexibleContexts #-}

module Poreus.SessionSpec (spec) where

import qualified Control.Monad.State.Strict as MS
import Data.Text (Text)
import Database.SQLite.Simple (Connection)
import Test.Hspec

import Poreus.Effects.FileSystem (removeFile)
import Poreus.Identity (Identity (..), resolveIdentityFrom)
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
  addProc 500 (ProcInfo (Just 200) "poreus" True 111)
  setEnv "CLAUDE_CONFIG_DIR" "/cfg"
  addFile
    "/cfg/sessions/200.json"
    ( "{\"pid\":200,\"sessionId\":\"abc\",\"cwd\":\"/ws/alice\",\"procStart\":\"20\"\
      \,\"status\":\"idle\",\"statusUpdatedAt\":1787081924146,\"name\":\""
        <> name
        <> "\"}"
    )

-- | Two identity-map rows for session "alice": an older one naming
-- claude pid 199, then the live window at pid 200. @oldAlive@ decides
-- whether 199 is still running, which separates the liveness filter
-- from the ordering.
twoMapRows :: Connection -> Text -> Bool -> TestIOM ()
twoMapRows c liveName oldAlive = do
  setMyPid 100
  addProc 100 (ProcInfo (Just 199) "poreus" True 10)
  addProc 199 (ProcInfo Nothing "claude" oldAlive 19)
  setEnv "CLAUDE_CONFIG_DIR" "/cfg"
  addFile "/cfg/sessions/199.json" "{\"pid\":199,\"name\":\"old-window\"}"
  _ <- resolveIdentityFrom c (Just "alice") "/ws/alice"
  advanceClock 60
  claudeHost liveName
  _ <- resolveIdentityFrom c (Just "alice") "/ws/alice"
  pure ()

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

  describe "the host's name is read, never stored (OQ-4)" $ do
    it "resolves the host's name for a session through the identity map" $ do
      (name, _) <- withTestDB initialTestState $ \c -> do
        claudeHost "redesign"
        addr <- idAddress <$> resolveIdentityFrom c (Just "alice") "/ws/alice"
        _ <- ensureSession c addr "/ws/alice" (Just 500) (Just "boot-test")
        liveHostNameOf c addr
      name `shouldBe` Just "redesign"

    it "follows a mid-session rename with no contact in between" $ do
      -- This is the whole reason it is not stored. A stored copy is
      -- renewed when the session is ACTIVE, and every consumer of it
      -- describes a session that is IDLE.
      (name, _) <- withTestDB initialTestState $ \c -> do
        claudeHost "poreus-transport"
        addr <- idAddress <$> resolveIdentityFrom c (Just "alice") "/ws/alice"
        _ <- ensureSession c addr "/ws/alice" (Just 500) (Just "boot-test")
        -- The user renames it; the session stays idle, so nothing in
        -- poreus is touched between the rename and the read.
        addFile "/cfg/sessions/200.json" "{\"pid\":200,\"name\":\"redesign\"}"
        liveHostNameOf c addr
      name `shouldBe` Just "redesign"

    it "resolves from the process tree with no identity-map row at all" $ do
      -- The map is written when a session CONTACTS poreus; the
      -- doorbell is for a session that is IDLE, so the map can be
      -- absent exactly when it is needed. Measured 2026-08-26: a
      -- window started at 09:24:43 was unringable until 09:31:08,
      -- because the row naming its pid did not exist yet. The serve
      -- pid's parent is the answer and needs no row.
      (name, _) <- withTestDB initialTestState $ \c -> do
        claudeHost "redesign"
        _ <- ensureSession c alice "/ws/alice" (Just 500) (Just "boot-test")
        liveHostNameOf c alice
      name `shouldBe` Just "redesign"

    it "is Nothing when neither the tree nor the map can name it" $ do
      -- A hook-only row: no serve pid to walk up from, and the address
      -- was never seeded into the map.
      (name, _) <- withTestDB initialTestState $ \c -> do
        claudeHost "redesign"
        _ <- ensureSession c alice "/ws/alice" Nothing Nothing
        liveHostNameOf c alice
      name `shouldBe` Nothing

    it "skips a map row that names a dead claude process" $ do
      -- `host_sessions` is keyed by process instance, not by session
      -- (ADR-0016 §2), so one session id carries a row per claude
      -- process that ever presented it — `claude --resume` in a fresh
      -- window adds one. The old reverse lookup was a single-valued
      -- `lookup` over an unordered SELECT, so the oldest row won.
      -- Measured 2026-08-26: 6 such session ids on one host, and in
      -- all 6 the first row was the dead one.
      (name, _) <- withTestDB initialTestState $ \c -> do
        twoMapRows c "redesign" False
        -- No serve pid, so only the map can answer.
        _ <- ensureSession c alice "/ws/alice" Nothing Nothing
        liveHostNameOf c alice
      name `shouldBe` Just "redesign"

    it "prefers the newest map row when both name a live process" $ do
      (name, _) <- withTestDB initialTestState $ \c -> do
        twoMapRows c "redesign" True
        _ <- ensureSession c alice "/ws/alice" Nothing Nothing
        liveHostNameOf c alice
      name `shouldBe` Just "redesign"

    it "ignores a map row from an earlier boot" $ do
      -- (pid, boot) is the identity; the same pid in another boot is an
      -- unrelated process.
      (name, _) <- withTestDB initialTestState $ \c -> do
        claudeHost "redesign"
        _ <- resolveIdentityFrom c (Just "alice") "/ws/alice"
        _ <- ensureSession c alice "/ws/alice" Nothing Nothing
        setBootId "boot-2"
        liveHostNameOf c alice
      name `shouldBe` Nothing

    it "is Nothing when the host publishes no file for the claude process" $ do
      (name, _) <- withTestDB initialTestState $ \c -> do
        claudeHost "redesign"
        addr <- idAddress <$> resolveIdentityFrom c (Just "alice") "/ws/alice"
        _ <- ensureSession c addr "/ws/alice" (Just 500) (Just "boot-test")
        removeFile "/cfg/sessions/200.json"
        liveHostNameOf c addr
      name `shouldBe` Nothing

    it "resolves a whole listing" $ do
      (names, _) <- withTestDB initialTestState $ \c -> do
        claudeHost "redesign"
        addr <- idAddress <$> resolveIdentityFrom c (Just "alice") "/ws/alice"
        _ <- ensureSession c addr "/ws/alice" (Just 500) (Just "boot-test")
        hostNamesByAddress c
      map snd names `shouldBe` ["redesign"]

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
