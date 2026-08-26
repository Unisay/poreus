{-# LANGUAGE FlexibleContexts #-}

module Poreus.DoctorSpec (spec) where

import qualified Control.Monad.State.Strict as MS
import qualified Data.ByteString as BS
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection)
import Test.Hspec

import Poreus.Doctor
import Poreus.Effects.FileSystem (removeFile)
import Poreus.Identity (Identity (..), resolveIdentityFrom)
import Poreus.Name (claimName, releaseName)
import Poreus.Post (Sender (..), postRequest)
import Poreus.Retention (sweepIfDue)
import Poreus.Session (ensureSession)
import Poreus.TestM
import Poreus.Types

alice, bob :: SessionAddress
alice = SessionAddress "s-alice"
bob = SessionAddress "s-bob"

-- | A claude host process at pid 200 publishing `name`, with a status
-- timestamp `ageHours` old relative to the fake epoch, and a
-- `poreus serve` child at pid 500.
--
-- Note [The fixture must not publish under the serve pid]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The host keys its session files by the CLAUDE pid (200 here);
-- `sessions.pid` holds the SERVE pid (500). The first version of this
-- fixture wrote a file under 500 as well, which made both host
-- comparisons pass while the shipped code compared the wrong
-- namespace — its two host checks were false on every real session and
-- the tests never noticed. Publishing only under 200, as the host
-- actually does, is what makes these tests mean anything.
claudeHost :: MS.MonadState TestState m => Text -> Double -> m ()
claudeHost name ageHours = do
  setMyPid 100
  addProc 100 (ProcInfo (Just 200) "poreus" True 10)
  addProc 200 (ProcInfo Nothing "claude" True 20)
  addProc 500 (ProcInfo (Just 200) "poreus" True 111)
  setEnv "CLAUDE_CONFIG_DIR" "/cfg"
  addFile
    "/cfg/sessions/200.json"
    ( "{\"pid\":200,\"status\":\"idle\",\"statusUpdatedAt\":"
        <> T.pack (show (epochMillis - round (ageHours * 3600 * 1000) :: Integer))
        <> ",\"name\":\""
        <> name
        <> "\"}"
    )

-- | Seed the identity map the way a real server start does, so a
-- session row can be joined back to its claude process.
seedIdentity :: Connection -> Text -> TestIOM SessionAddress
seedIdentity c sid = do
  identity <- resolveIdentityFrom c (Just sid) "/ws/alice"
  pure (idAddress identity)

-- | 2026-01-01T00:00:00Z, the fake clock's epoch, in milliseconds.
epochMillis :: Integer
epochMillis = 1767225600000

findCheck :: Text -> [Finding] -> Maybe Finding
findCheck name = find ((== name) . fCheck)

spec :: Spec
spec = do
  describe "diagnose: presence" $ do
    it "is an error when poreus reads live on a pid the host does not publish" $ do
      -- The false-alive direction, the one that misroutes messages.
      (fs, _) <- withTestDB initialTestState $ \c -> do
        addProc 500 (ProcInfo Nothing "poreus" True 111)
        setEnv "CLAUDE_CONFIG_DIR" "/cfg"
        _ <- ensureSession c alice "/ws/alice" (Just 500) (Just "boot-test")
        diagnose c
      fmap fSeverity (findCheck "presence" fs) `shouldBe` Just SevError

    it "warns about a row that never recorded a pid, without blaming the server" $ do
      -- The gap this design knowingly accepts: the hook creates such a
      -- row, and a session killed without shutting down leaves it
      -- reading live. In a mixed-version fleet it also fires for every
      -- peer whose server writes a different store, so the wording must
      -- not assert that anything is broken.
      (fs, _) <- withTestDB initialTestState $ \c -> do
        _ <- ensureSession c alice "/ws/alice" Nothing Nothing
        diagnose c
      case findCheck "presence" fs of
        Just f -> do
          fSeverity f `shouldBe` SevWarn
          fDetail f `shouldSatisfy` T.isInfixOf "in this store"
          fDetail f `shouldSatisfy` T.isInfixOf "its server writes elsewhere"
        Nothing -> expectationFailure "expected a presence finding"

    it "says nothing when the pid is published by the host" $ do
      (fs, _) <- withTestDB initialTestState $ \c -> do
        claudeHost "redesign" 0
        addr <- seedIdentity c "alice"
        _ <- ensureSession c addr "/ws/alice" (Just 500) (Just "boot-test")
        diagnose c
      findCheck "presence" fs `shouldBe` Nothing

  describe "diagnose: the label bridges roles and windows" $ do
    it "names the roles a session serves, so an operator can search by role" $ do
      -- An operator opens doctor because a ROLE is misbehaving. If the
      -- output names only the window, the word they searched for
      -- appears nowhere and they must already know the answer.
      (fs, _) <- withTestDB initialTestState $ \c -> do
        claudeHost "deployer" 0
        addr <- seedIdentity c "alice"
        _ <- ensureSession c addr "/ws/alice" (Just 500) (Just "boot-test")
        _ <- claimName c addr "nixos" False
        diagnose c
      case findCheck "status" fs of
        Just f -> do
          fDetail f `shouldSatisfy` T.isInfixOf "the host calls it 'deployer'"
          fDetail f `shouldSatisfy` T.isInfixOf "serving 'nixos'"
        Nothing -> expectationFailure "expected a status finding"

    it "omits the serving clause when the session holds no role" $ do
      (fs, _) <- withTestDB initialTestState $ \c -> do
        claudeHost "deployer" 0
        addr <- seedIdentity c "alice"
        _ <- ensureSession c addr "/ws/alice" (Just 500) (Just "boot-test")
        diagnose c
      case findCheck "status" fs of
        Just f -> fDetail f `shouldSatisfy` (not . T.isInfixOf "serving")
        Nothing -> expectationFailure "expected a status finding"

  describe "diagnose: an unresolvable host says which fault it is" $ do
    it "says so when poreus never learned which claude process a session is" $ do
      -- No serve pid to walk up from and no `host_sessions` row.
      -- Distinct from the file having gone away, and an operator wants
      -- to know which.
      (fs, _) <- withTestDB initialTestState $ \c -> do
        _ <- ensureSession c alice "/ws/alice" Nothing Nothing
        diagnose c
      case findCheck "host-name" fs of
        Just f -> fDetail f `shouldSatisfy` T.isInfixOf "no live claude process"
        Nothing -> expectationFailure "expected a host-name finding"

    it "reports a known claude pid whose session file is absent" $ do
      (fs, _) <- withTestDB initialTestState $ \c -> do
        claudeHost "deployer" 0
        addr <- seedIdentity c "alice"
        _ <- ensureSession c addr "/ws/alice" Nothing Nothing
        removeFile "/cfg/sessions/200.json"
        diagnose c
      case findCheck "host-name" fs of
        Just f -> do
          fDetail f `shouldSatisfy` T.isInfixOf "belongs to claude pid 200"
          fDetail f `shouldSatisfy` T.isInfixOf "no session file"
        Nothing -> expectationFailure "expected a host-name finding"

  describe "diagnose: the host join agrees with the doorbell" $ do
    it "is not fooled by an older map row naming a dead claude process" $ do
      -- Doctor read the identity map with a single-valued `lookup` over
      -- an unordered SELECT, so it took the oldest row. Measured
      -- 2026-08-26 on this host: it called 8 of 9 live, healthy, named
      -- sessions broken and exited non-zero. A check that cries wolf on
      -- every session is worse than no check.
      (fs, _) <- withTestDB initialTestState $ \c -> do
        -- The window that has since exited, recorded first.
        setMyPid 100
        addProc 100 (ProcInfo (Just 199) "poreus" True 10)
        addProc 199 (ProcInfo Nothing "claude" False 19)
        setEnv "CLAUDE_CONFIG_DIR" "/cfg"
        addr <- seedIdentity c "alice"
        advanceClock 60
        -- The window running now, recorded second.
        claudeHost "redesign" 0
        _ <- seedIdentity c "alice"
        _ <- ensureSession c addr "/ws/alice" (Just 500) (Just "boot-test")
        diagnose c
      findCheck "presence" fs `shouldBe` Nothing
      case findCheck "status" fs of
        Just f -> fDetail f `shouldSatisfy` T.isInfixOf "the host calls it 'redesign'"
        Nothing -> expectationFailure "expected a status finding"

    it "sees a session belonging to the other host profile" $ do
      -- One poreus store serves several host profiles, and their session
      -- files do not share a directory. Reading our own profile made
      -- doctor call three live personal-profile sessions broken on
      -- 2026-08-26, while their files sat one directory over.
      (fs, _) <- withTestDB initialTestState $ \c -> do
        setMyPid 100
        addProc 100 (ProcInfo (Just 200) "poreus" True 10)
        addProc 200 (ProcInfo Nothing "claude" True 20)
        addProc 500 (ProcInfo (Just 200) "poreus" True 111)
        setEnv "CLAUDE_CONFIG_DIR" "/work"
        setProcEnv 200 "CLAUDE_CONFIG_DIR" "/personal"
        addFile
          "/personal/sessions/200.json"
          ( "{\"pid\":200,\"status\":\"idle\",\"statusUpdatedAt\":"
              <> T.pack (show epochMillis)
              <> ",\"name\":\"tomb2-window\"}"
          )
        addr <- seedIdentity c "alice"
        _ <- ensureSession c addr "/ws/alice" (Just 500) (Just "boot-test")
        diagnose c
      findCheck "presence" fs `shouldBe` Nothing
      case findCheck "status" fs of
        Just f -> fDetail f `shouldSatisfy` T.isInfixOf "the host calls it 'tomb2-window'"
        Nothing -> expectationFailure "expected a status finding"

  describe "diagnose: the two session ids" $ do
    it "reports a host session id that has moved on from the address" $ do
      -- Expected by ADR-0016 and reported anyway: two different UUIDs
      -- for one window is the shape an operator opens an investigation
      -- over, and on 2026-08-26 one did.
      (fs, _) <- withTestDB initialTestState $ \c -> do
        claudeHost "redesign" 0
        addr <- seedIdentity c "alice"
        _ <- ensureSession c addr "/ws/alice" (Just 500) (Just "boot-test")
        addFile
          "/cfg/sessions/200.json"
          "{\"pid\":200,\"name\":\"redesign\",\"sessionId\":\"cleared\"}"
        diagnose c
      case findCheck "identity" fs of
        Just f -> do
          fSeverity f `shouldBe` SevOk
          fDetail f `shouldSatisfy` T.isInfixOf "addressed as 'alice'"
          fDetail f `shouldSatisfy` T.isInfixOf "'cleared'"
          fDetail f `shouldSatisfy` T.isInfixOf "ADR-0016"
        Nothing -> expectationFailure "expected an identity finding"

    it "says nothing when the two agree" $ do
      (fs, _) <- withTestDB initialTestState $ \c -> do
        claudeHost "redesign" 0
        addr <- seedIdentity c "alice"
        _ <- ensureSession c addr "/ws/alice" (Just 500) (Just "boot-test")
        addFile
          "/cfg/sessions/200.json"
          "{\"pid\":200,\"name\":\"redesign\",\"sessionId\":\"alice\"}"
        diagnose c
      findCheck "identity" fs `shouldBe` Nothing

  describe "diagnose: build skew" $ do
    it "warns when a live server runs a different build from the CLI" $ do
      -- Measured 2026-08-26, right after deploying the delivery fix:
      -- doctor exited 0 while all 10 live serve processes still ran the
      -- previous build, so no session on the host had the fix in force.
      -- Doctor's other findings were all correct; the conclusion an
      -- operator draws from them was not.
      (fs, _) <- withTestDB initialTestState $ \c -> do
        claudeHost "redesign" 0
        setProcExe 100 "/nix/store/new-poreus/bin/poreus"
        setProcExe 500 "/nix/store/old-poreus/bin/poreus"
        addr <- seedIdentity c "alice"
        _ <- ensureSession c addr "/ws/alice" (Just 500) (Just "boot-test")
        diagnose c
      case findCheck "build" fs of
        Just f -> do
          fSeverity f `shouldBe` SevWarn
          fDetail f `shouldSatisfy` T.isInfixOf "1 of 1"
          fDetail f `shouldSatisfy` T.isInfixOf "/nix/store/old-poreus/bin/poreus"
          fDetail f `shouldSatisfy` T.isInfixOf "/nix/store/new-poreus/bin/poreus"
        Nothing -> expectationFailure "expected a build finding"

    it "sorts the build warning above other warnings of equal severity" $ do
      -- It says whether the reader may generalise from the rest, so it
      -- must not be buried among them.
      (fs, _) <- withTestDB initialTestState $ \c -> do
        claudeHost "redesign" 48
        setProcExe 100 "/nix/store/new-poreus/bin/poreus"
        setProcExe 500 "/nix/store/old-poreus/bin/poreus"
        addr <- seedIdentity c "alice"
        _ <- ensureSession c addr "/ws/alice" (Just 500) (Just "boot-test")
        diagnose c
      map fCheck (filter ((== SevWarn) . fSeverity) fs) `shouldStartWith` ["build"]

    it "is ok when every live server runs this build" $ do
      (fs, _) <- withTestDB initialTestState $ \c -> do
        claudeHost "redesign" 0
        setProcExe 100 "/nix/store/same-poreus/bin/poreus"
        setProcExe 500 "/nix/store/same-poreus/bin/poreus"
        addr <- seedIdentity c "alice"
        _ <- ensureSession c addr "/ws/alice" (Just 500) (Just "boot-test")
        diagnose c
      fmap fSeverity (findCheck "build" fs) `shouldBe` Just SevOk

    it "says so rather than guessing when it cannot read its own executable" $ do
      -- A silent Nothing would restore the false clean bill this check
      -- exists to prevent.
      (fs, _) <- withTestDB initialTestState $ \c -> do
        claudeHost "redesign" 0
        setProcExe 500 "/nix/store/old-poreus/bin/poreus"
        addr <- seedIdentity c "alice"
        _ <- ensureSession c addr "/ws/alice" (Just 500) (Just "boot-test")
        diagnose c
      case findCheck "build" fs of
        Just f -> do
          fSeverity f `shouldBe` SevOk
          fDetail f `shouldSatisfy` T.isInfixOf "cannot read this process's own executable"
        Nothing -> expectationFailure "expected a build finding"

  describe "diagnose: status staleness" $ do
    it "reports a live pid whose host status stopped moving" $ do
      -- The replacement for v0.3's stale-heartbeat check. The
      -- difference: the staleness is the host's now, not ours.
      (fs, _) <- withTestDB initialTestState $ \c -> do
        claudeHost "redesign" 48
        addr <- seedIdentity c "alice"
        _ <- ensureSession c addr "/ws/alice" (Just 500) (Just "boot-test")
        diagnose c
      case findCheck "status" fs of
        Just f -> do
          fSeverity f `shouldBe` SevWarn
          fDetail f `shouldSatisfy` T.isInfixOf "48 h ago"
        Nothing -> expectationFailure "expected a status finding"

    it "accepts an idle session that the host touched recently" $ do
      (fs, _) <- withTestDB initialTestState $ \c -> do
        claudeHost "redesign" 2
        addr <- seedIdentity c "alice"
        _ <- ensureSession c addr "/ws/alice" (Just 500) (Just "boot-test")
        diagnose c
      fmap fSeverity (findCheck "status" fs) `shouldBe` Just SevOk

  describe "diagnose: backlog" $ do
    it "warns about mail queued for a role nobody holds" $ do
      (fs, _) <- withTestDB initialTestState $ \c -> do
        claudeHost "redesign" 0
        addr <- seedIdentity c "alice"
        _ <- ensureSession c addr "/ws/alice" (Just 500) (Just "boot-test")
        _ <- ensureSession c bob "/ws/bob" Nothing Nothing
        _ <- claimName c bob "nixos" False
        setRandomInts [0 ..]
        _ <- postRequest c (Sender addr Nothing) "nixos" "work" Nothing Nothing False
        _ <- releaseName c bob
        diagnose c
      case findCheck "backlog" fs of
        Just f -> do
          fSeverity f `shouldBe` SevWarn
          fDetail f `shouldSatisfy` T.isInfixOf "no session holds it"
        Nothing -> expectationFailure "expected a backlog finding"

    it "does not fault a held role whose holder has not read yet" $ do
      (fs, _) <- withTestDB initialTestState $ \c -> do
        claudeHost "redesign" 0
        addr <- seedIdentity c "alice"
        _ <- ensureSession c addr "/ws/alice" (Just 500) (Just "boot-test")
        _ <- ensureSession c bob "/ws/bob" Nothing Nothing
        _ <- claimName c bob "nixos" False
        setRandomInts [0 ..]
        _ <- postRequest c (Sender addr Nothing) "nixos" "work" Nothing Nothing False
        diagnose c
      fmap fSeverity (findCheck "backlog" fs) `shouldBe` Just SevOk

  describe "diagnose: retention" $ do
    it "warns when no sweep has ever run" $ do
      -- A stalled sweep took days to show up in v0.3, as a 4.1 MB WAL.
      (fs, _) <- withTestDB initialTestState diagnose
      case findCheck "retention" fs of
        Just f -> do
          fSeverity f `shouldBe` SevWarn
          fDetail f `shouldSatisfy` T.isInfixOf "no sweep has ever run"
        Nothing -> expectationFailure "expected a retention finding"

    it "is satisfied once a sweep has run recently" $ do
      (fs, _) <- withTestDB initialTestState $ \c -> do
        _ <- sweepIfDue c 30
        diagnose c
      fmap fSeverity (findCheck "retention" fs) `shouldBe` Just SevOk

    it "warns when the last sweep is more than a day old" $ do
      (fs, _) <- withTestDB initialTestState $ \c -> do
        _ <- sweepIfDue c 30
        advanceClock (2 * 86400)
        diagnose c
      fmap fSeverity (findCheck "retention" fs) `shouldBe` Just SevWarn

  describe "diagnose: write-ahead log" $ do
    it "warns when the log has outgrown its checkpoints" $ do
      (fs, _) <- withTestDB initialTestState $ \c -> do
        setEnv "POREUS_HOME" "/p"
        addBytes "/p/db-v4.sqlite-wal" (bigBlob (5 * 1024 * 1024))
        diagnose c
      case findCheck "wal" fs of
        Just f -> do
          fSeverity f `shouldBe` SevWarn
          fDetail f `shouldSatisfy` T.isInfixOf "5 MB"
        Nothing -> expectationFailure "expected a wal finding"

    it "is quiet with no log on disk" $ do
      (fs, _) <- withTestDB initialTestState $ \c -> do
        setEnv "POREUS_HOME" "/p"
        diagnose c
      fmap fSeverity (findCheck "wal" fs) `shouldBe` Just SevOk

  describe "renderFinding" $ do
    it "leads with the severity so a scan finds the errors" $ do
      renderFinding (Finding SevError "host-name" "drifted")
        `shouldSatisfy` T.isPrefixOf "ERROR"

  describe "ordering" $ do
    it "puts the worst finding first" $ do
      (fs, _) <- withTestDB initialTestState $ \c -> do
        addProc 500 (ProcInfo Nothing "poreus" True 111)
        setEnv "CLAUDE_CONFIG_DIR" "/cfg"
        _ <- ensureSession c alice "/ws/alice" (Just 500) (Just "boot-test")
        diagnose c
      fmap fSeverity (headMay fs) `shouldBe` Just SevError

bigBlob :: Int -> BS.ByteString
bigBlob n = BS.replicate n 0

headMay :: [a] -> Maybe a
headMay (x : _) = Just x
headMay [] = Nothing
