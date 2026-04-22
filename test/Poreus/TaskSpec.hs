module Poreus.TaskSpec (spec) where

import qualified Data.Aeson as A
import qualified Data.Text as T
import Data.Time (addUTCTime, defaultTimeLocale, parseTimeOrError)
import Test.Hspec

import Poreus.Profile (registerAgent)
import Poreus.Task
import Poreus.TestM
import Poreus.Time (Timestamp (..))
import Poreus.Types

spec :: Spec
spec = do
  describe "newTaskId" $ do
    it "formats YYYYMMDD-HHmmss-<from>-<hex>" $ do
      let t = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Z"
                "2026-04-22T13:05:09Z"
      unTaskId (newTaskId "alice" (Timestamp t) "abcd")
        `shouldBe` "20260422-130509-alice-abcd"

  describe "sendTask + loadTask" $ do
    it "round-trips a freetext task in pending state" $ do
      let t0 = tsClock emptyTestState
          tid = newTaskId "a" (Timestamp t0) "beef"
          input =
            SendInput
              { siTo = "b"
              , siKind = KindFreetext
              , siUrl = Nothing
              , siDescription = Just "do a thing"
              , siExpected = Just "it's done"
              }
      (loaded, _) <- withMemoryDB emptyTestState $ \c -> do
        _ <- registerAgent c "a" "/tmp/a" (Timestamp t0)
        _ <- sendTask c "a" tid (Timestamp t0) input
        loadTask c tid
      case loaded of
        Just t -> do
          taskId t `shouldBe` tid
          taskFrom t `shouldBe` "a"
          taskTo t `shouldBe` "b"
          taskStatus t `shouldBe` TSPending
          taskCreatedAt t `shouldBe` Timestamp t0
          taskDescription t `shouldBe` Just "do a thing"
          taskExpected t `shouldBe` Just "it's done"
        Nothing -> expectationFailure "task not loaded"

  describe "claim/complete happy path" $ do
    it "drives a task through pending → claimed → completed" $ do
      let t0 = tsClock emptyTestState
          t1 = addUTCTime 10 t0
          t2 = addUTCTime 20 t0
          tid = newTaskId "a" (Timestamp t0) "0001"
          input =
            SendInput
              { siTo = "b"
              , siKind = KindFreetext
              , siUrl = Nothing
              , siDescription = Nothing
              , siExpected = Nothing
              }
          complete =
            CompleteInput
              { ciStatus = RSCompleted
              , ciSummary = Just "fine"
              , ciArtifacts = Just (A.toJSON [1 :: Int, 2])
              }
      let rep = newTaskId "a" (Timestamp t2) "rrrr"
      (out, _) <- withMemoryDB emptyTestState $ \c -> do
        _ <- registerAgent c "a" "/a" (Timestamp t0)
        _ <- sendTask c "a" tid (Timestamp t0) input
        cr <- claimTask c tid (Timestamp t1)
        co <- completeTask c tid rep (Timestamp t2) complete
        r <- loadResult c tid
        pure (cr, co, r)
      case out of
        (Right claimed, Right (finished, result), Just storedResult) -> do
          taskStatus claimed `shouldBe` TSClaimed
          taskClaimedAt claimed `shouldBe` Just (Timestamp t1)
          taskStatus finished `shouldBe` TSCompleted
          taskCompletedAt finished `shouldBe` Just (Timestamp t2)
          resultStatus result `shouldBe` RSCompleted
          resultStatus storedResult `shouldBe` RSCompleted
          resultSummary storedResult `shouldBe` Just "fine"
          resultArtifacts storedResult `shouldBe` A.toJSON [1 :: Int, 2]
        _ -> expectationFailure "expected a full happy-path result"

  describe "illegal transitions" $ do
    it "cannot claim a task twice" $ do
      let t0 = tsClock emptyTestState
          tid = newTaskId "a" (Timestamp t0) "0002"
      (res, _) <- withMemoryDB emptyTestState $ \c -> do
        _ <- registerAgent c "a" "/a" (Timestamp t0)
        _ <-
          sendTask
            c
            "a"
            tid
            (Timestamp t0)
            (SendInput "b" KindFreetext Nothing Nothing Nothing)
        _ <- claimTask c tid (Timestamp t0)
        claimTask c tid (Timestamp t0)
      case res of
        Left (TaskBadTransition msg) ->
          msg `shouldSatisfy` T.isInfixOf "claim"
        _ -> expectationFailure "expected TaskBadTransition"

    it "cannot complete a pending task" $ do
      let t0 = tsClock emptyTestState
          tid = newTaskId "a" (Timestamp t0) "0003"
          rep = newTaskId "a" (Timestamp t0) "r003"
          complete =
            CompleteInput
              {ciStatus = RSCompleted, ciSummary = Nothing, ciArtifacts = Nothing}
      (res, _) <- withMemoryDB emptyTestState $ \c -> do
        _ <- registerAgent c "a" "/a" (Timestamp t0)
        _ <- sendTask c "a" tid (Timestamp t0)
              (SendInput "b" KindFreetext Nothing Nothing Nothing)
        completeTask c tid rep (Timestamp t0) complete
      case res of
        Left (TaskBadTransition _) -> pure ()
        _ -> expectationFailure "expected TaskBadTransition"

    it "reports TaskNotFound for unknown id" $ do
      (res, _) <- withMemoryDB emptyTestState $ \c ->
        claimTask c "nosuch" (Timestamp (tsClock emptyTestState))
      case res of
        Left (TaskNotFound _) -> pure ()
        _ -> expectationFailure "expected TaskNotFound"

  describe "reject" $ do
    it "rejects a pending task with a reason" $ do
      let t0 = tsClock emptyTestState
          tid = newTaskId "a" (Timestamp t0) "0004"
          rep = newTaskId "b" (Timestamp t0) "r004"
      (res, _) <- withMemoryDB emptyTestState $ \c -> do
        _ <- registerAgent c "a" "/a" (Timestamp t0)
        _ <- sendTask c "a" tid (Timestamp t0)
              (SendInput "b" KindFreetext Nothing Nothing Nothing)
        rejectTask c tid rep (Timestamp t0) "busy"
      case res of
        Right (t, r) -> do
          taskStatus t `shouldBe` TSRejected
          resultStatus r `shouldBe` RSRejected
          resultSummary r `shouldBe` Just "busy"
        Left _ -> expectationFailure "expected successful reject"

  describe "inbox/sent" $ do
    it "distinguishes sent and received sides" $ do
      let t0 = tsClock emptyTestState
      (out, _) <- withMemoryDB emptyTestState $ \c -> do
        _ <- registerAgent c "a" "/a" (Timestamp t0)
        _ <- registerAgent c "b" "/b" (Timestamp t0)
        let tid1 = newTaskId "a" (Timestamp t0) "0010"
            tid2 = newTaskId "b" (Timestamp t0) "0011"
        _ <- sendTask c "a" tid1 (Timestamp t0)
              (SendInput "b" KindFreetext Nothing Nothing Nothing)
        _ <- sendTask c "b" tid2 (Timestamp t0)
              (SendInput "a" KindFreetext Nothing Nothing Nothing)
        aSent <- sentTasks c "a"
        aRecv <- inboxTasks c "a" Nothing
        pure (map taskId aSent, map taskId aRecv)
      out `shouldBe` (["20260101-000000-a-0010"], ["20260101-000000-b-0011"])
