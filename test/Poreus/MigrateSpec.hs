module Poreus.MigrateSpec (spec) where

import qualified Data.Aeson as A
import Data.Time (defaultTimeLocale, parseTimeOrError)
import Test.Hspec

import Poreus.Endpoint (loadAllAgents)
import Poreus.Migrate
import Poreus.Task (loadResult, loadTask)
import Poreus.TestM
import Poreus.Types

spec :: Spec
spec = do
  describe "extractRepos" $ do
    it "lifts every entry of a well-formed registry" $ do
      let v = case A.decode "{\"repos\":{\"a\":{\"path\":\"/a\",\"registered_at\":\"2026-01-01T00:00:00Z\"},\"b\":{\"path\":\"/b\"}}}" of
            Just x -> x
            Nothing -> error "fixture"
      case extractRepos v of
        Just rows ->
          rows
            `shouldMatchList` [ ("a", "/a", "2026-01-01T00:00:00Z")
                              , ("b", "/b", "")
                              ]
        Nothing -> expectationFailure "expected Just"

    it "returns Nothing when the shape is wrong" $ do
      extractRepos (A.String "not an object") `shouldBe` Nothing
      extractRepos (case A.decode "{}" of Just x -> x; Nothing -> error "x") `shouldBe` Nothing

  describe "extractTask" $ do
    it "parses the common legacy fields" $ do
      let v = case A.decode "{\"task_id\":\"20260101-000000-a-abcd\",\"from\":\"a\",\"to\":\"b\",\"status\":\"pending\",\"created_at\":\"2026-01-01T00:00:00Z\",\"description\":\"d\",\"expected_result\":\"e\"}" of
            Just x -> x
            Nothing -> error "fixture"
      extractTask v
        `shouldBe` Just
          ( "20260101-000000-a-abcd"
          , "a"
          , "b"
          , "freetext"
          , "2026-01-01T00:00:00Z"
          , Just "d"
          , Just "e"
          , Nothing
          , "pending"
          )

    it "accepts {from: {alias: ...}} shape too" $ do
      let v = case A.decode "{\"task_id\":\"t\",\"from\":{\"alias\":\"a\",\"path\":\"/a\"},\"to\":{\"alias\":\"b\"}}" of
            Just x -> x
            Nothing -> error "fixture"
      case extractTask v of
        Just (_, from, to, _, _, _, _, _, _) -> (from, to) `shouldBe` ("a", "b")
        Nothing -> expectationFailure "expected Just"

    it "accepts \"id\" as an alias for \"task_id\" (field drift in the legacy store)" $ do
      let v = case A.decode "{\"id\":\"20260420-110856-nixos-8c3e\",\"from\":\"nixos\",\"to\":\"alice\"}" of
            Just x -> x
            Nothing -> error "fixture"
      case extractTask v of
        Just (tid, _, _, _, _, _, _, _, _) ->
          tid `shouldBe` "20260420-110856-nixos-8c3e"
        Nothing -> expectationFailure "expected Just"

    it "ignores unknown fields like updated_at" $ do
      let v = case A.decode "{\"id\":\"t1\",\"from\":\"a\",\"to\":\"b\",\"updated_at\":\"2026-04-22T00:00:00Z\"}" of
            Just x -> x
            Nothing -> error "fixture"
      case extractTask v of
        Just (tid, from, to, _, _, _, _, _, _) ->
          (tid, from, to) `shouldBe` ("t1", "a", "b")
        Nothing -> expectationFailure "expected Just"

    it "rejects non-object" $ do
      extractTask (A.Null) `shouldBe` Nothing

  describe "extractResult" $ do
    it "pulls out task_id, status, summary, artifacts, completed_at" $ do
      let v = case A.decode "{\"task_id\":\"t\",\"status\":\"completed\",\"summary\":\"s\",\"artifacts\":[1,2],\"completed_at\":\"2026-01-01T00:00:00Z\"}" of
            Just x -> x
            Nothing -> error "fixture"
      case extractResult v of
        Just (tid, stat, summ, arts, doneAt) -> do
          tid `shouldBe` "t"
          stat `shouldBe` "completed"
          summ `shouldBe` Just "s"
          doneAt `shouldBe` "2026-01-01T00:00:00Z"
          arts `shouldBe` A.toJSON [1 :: Int, 2]
        Nothing -> expectationFailure "expected Just"

    it "defaults missing status to completed and missing artifacts to []" $ do
      let v = case A.decode "{\"task_id\":\"t\"}" of Just x -> x; Nothing -> error "x"
      case extractResult v of
        Just (_, stat, _, arts, _) -> do
          stat `shouldBe` "completed"
          arts `shouldBe` A.Array mempty
        Nothing -> expectationFailure "expected Just"

  describe "migrateFromLegacy (end-to-end on fake FS + in-memory DB)" $ do
    it "ingests registry.json, inbox tasks and result files" $ do
      let t0 = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Z"
                 "2026-04-22T12:00:00Z"
          fixture = do
            setEnv "HOME" "/home/test"
            addDir "/home/test/.claude-work/a2a-queue"
            addFile
              "/home/test/.claude-work/a2a-queue/registry.json"
              "{\"repos\":{\"alice\":{\"path\":\"/a\",\"registered_at\":\"2026-01-01T00:00:00Z\"},\"bob\":{\"path\":\"/b\"}}}"
            addDir "/home/test/.claude-work/a2a-queue/inbox"
            addDir "/home/test/.claude-work/a2a-queue/inbox/alice"
            addFile
              "/home/test/.claude-work/a2a-queue/inbox/alice/20260101-000000-bob-0001.json"
              "{\"task_id\":\"20260101-000000-bob-0001\",\"from\":\"bob\",\"to\":\"alice\",\"status\":\"completed\",\"created_at\":\"2026-01-01T00:00:00Z\",\"description\":\"do a thing\"}"
            addDir "/home/test/.claude-work/a2a-queue/results"
            addFile
              "/home/test/.claude-work/a2a-queue/results/20260101-000000-bob-0001.json"
              "{\"task_id\":\"20260101-000000-bob-0001\",\"status\":\"completed\",\"summary\":\"ok\",\"completed_at\":\"2026-01-01T00:01:00Z\"}"
          st0 = execTestIOMOnly fixture emptyTestState
      (stats, _) <- withMemoryDB st0 $ \c -> do
        s <- migrateFromLegacy c t0
        agents <- loadAllAgents c
        mt <- loadTask c "20260101-000000-bob-0001"
        mr <- loadResult c "20260101-000000-bob-0001"
        pure (s, agents, mt, mr)
      case stats of
        (s, agents, Just t, Just r) -> do
          msAgents s `shouldBe` 2
          msTasks s `shouldBe` 1
          msResults s `shouldBe` 1
          map agentAlias agents `shouldMatchList` ["alice", "bob"]
          taskFrom t `shouldBe` "bob"
          taskTo t `shouldBe` "alice"
          taskStatus t `shouldBe` TSCompleted
          resultSummary r `shouldBe` Just "ok"
        _ -> expectationFailure "missing migration output"
  where
    -- tiny helper: execute a pure State setup against TestIOM's state
    execTestIOMOnly :: TestM () -> TestState -> TestState
    execTestIOMOnly = execTestM
