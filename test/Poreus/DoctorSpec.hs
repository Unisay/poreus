{-# LANGUAGE FlexibleContexts #-}

module Poreus.DoctorSpec (spec) where

import qualified Control.Monad.State.Strict as MS
import qualified Data.ByteString as BS
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Poreus.Doctor
import Poreus.Name (claimName, releaseName)
import Poreus.Post (Sender (..), postRequest)
import Poreus.Retention (sweepIfDue)
import Poreus.Session (ensureSession)
import Poreus.TestM
import Poreus.Types

alice, bob :: SessionAddress
alice = SessionAddress "s-alice"
bob = SessionAddress "s-bob"

-- | A claude host at pid 200 publishing `name` for our session, with a
-- status timestamp `ageHours` old relative to the fake epoch.
claudeHost :: MS.MonadState TestState m => Text -> Double -> m ()
claudeHost name ageHours = do
  setMyPid 100
  addProc 100 (ProcInfo (Just 200) "poreus" True 10)
  addProc 200 (ProcInfo Nothing "claude" True 20)
  addProc 500 (ProcInfo Nothing "poreus" True 111)
  setEnv "CLAUDE_CONFIG_DIR" "/cfg"
  addFile
    "/cfg/sessions/500.json"
    ( "{\"pid\":500,\"status\":\"idle\",\"statusUpdatedAt\":"
        <> T.pack (show (epochMillis - round (ageHours * 3600 * 1000) :: Integer))
        <> ",\"name\":\""
        <> name
        <> "\"}"
    )
  addFile "/cfg/sessions/200.json" ("{\"pid\":200,\"name\":\"" <> name <> "\"}")

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

    it "warns about a row that never recorded a pid" $ do
      -- The gap this design knowingly accepts: the hook creates such a
      -- row, and a session killed without shutting down leaves it
      -- reading live.
      (fs, _) <- withTestDB initialTestState $ \c -> do
        _ <- ensureSession c alice "/ws/alice" Nothing Nothing
        diagnose c
      fmap fSeverity (findCheck "presence" fs) `shouldBe` Just SevWarn

    it "says nothing when the pid is published by the host" $ do
      (fs, _) <- withTestDB initialTestState $ \c -> do
        claudeHost "redesign" 0
        _ <- ensureSession c alice "/ws/alice" (Just 500) (Just "boot-test")
        diagnose c
      findCheck "presence" fs `shouldBe` Nothing

  describe "diagnose: the host-name lease" $ do
    it "is an error when the host renamed the session behind the lease" $ do
      (fs, _) <- withTestDB initialTestState $ \c -> do
        claudeHost "poreus-transport" 0
        _ <- ensureSession c alice "/ws/alice" (Just 500) (Just "boot-test")
        -- The host renames, and nothing has run a hook since.
        addFile "/cfg/sessions/500.json" "{\"pid\":500,\"name\":\"redesign\"}"
        diagnose c
      case findCheck "host-name" fs of
        Just f -> do
          fSeverity f `shouldBe` SevError
          fDetail f `shouldSatisfy` T.isInfixOf "'poreus-transport'"
          fDetail f `shouldSatisfy` T.isInfixOf "'redesign'"
        Nothing -> expectationFailure "expected a host-name finding"

    it "is quiet when the lease matches" $ do
      (fs, _) <- withTestDB initialTestState $ \c -> do
        claudeHost "redesign" 0
        _ <- ensureSession c alice "/ws/alice" (Just 500) (Just "boot-test")
        diagnose c
      fmap fSeverity (findCheck "host-name" fs) `shouldBe` Nothing

  describe "diagnose: status staleness" $ do
    it "reports a live pid whose host status stopped moving" $ do
      -- The replacement for v0.3's stale-heartbeat check. The
      -- difference: the staleness is the host's now, not ours.
      (fs, _) <- withTestDB initialTestState $ \c -> do
        claudeHost "redesign" 48
        _ <- ensureSession c alice "/ws/alice" (Just 500) (Just "boot-test")
        diagnose c
      case findCheck "status" fs of
        Just f -> do
          fSeverity f `shouldBe` SevWarn
          fDetail f `shouldSatisfy` T.isInfixOf "48 h ago"
        Nothing -> expectationFailure "expected a status finding"

    it "accepts an idle session that the host touched recently" $ do
      (fs, _) <- withTestDB initialTestState $ \c -> do
        claudeHost "redesign" 2
        _ <- ensureSession c alice "/ws/alice" (Just 500) (Just "boot-test")
        diagnose c
      fmap fSeverity (findCheck "status" fs) `shouldBe` Just SevOk

  describe "diagnose: backlog" $ do
    it "warns about mail queued for a role nobody holds" $ do
      (fs, _) <- withTestDB initialTestState $ \c -> do
        claudeHost "redesign" 0
        _ <- ensureSession c alice "/ws/alice" (Just 500) (Just "boot-test")
        _ <- ensureSession c bob "/ws/bob" Nothing Nothing
        _ <- claimName c bob "nixos" False
        setRandomInts [0 ..]
        _ <- postRequest c (Sender alice Nothing) "nixos" "work" Nothing Nothing False
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
        _ <- ensureSession c alice "/ws/alice" (Just 500) (Just "boot-test")
        _ <- ensureSession c bob "/ws/bob" Nothing Nothing
        _ <- claimName c bob "nixos" False
        setRandomInts [0 ..]
        _ <- postRequest c (Sender alice Nothing) "nixos" "work" Nothing Nothing False
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
