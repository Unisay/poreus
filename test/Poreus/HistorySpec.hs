module Poreus.HistorySpec (spec) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, parseTimeOrError)
import Test.Hspec

import Poreus.History
import Poreus.Profile (registerAgent)
import Poreus.Task
import Poreus.TestM
import Poreus.Time (Timestamp (..))
import Poreus.Types

-- | 2026-04-24T08:14:06Z — matches the spec's example.
sampleTime :: UTCTime
sampleTime =
  parseTimeOrError
    True
    defaultTimeLocale
    "%Y-%m-%dT%H:%M:%S%Z"
    "2026-04-24T08:14:06Z"

spec :: Spec
spec = do
  describe "formatWhen" $ do
    it "slices to MM-DD HH:MM in UTC" $
      formatWhen sampleTime `shouldBe` "04-24 08:14"

  describe "firstLine" $ do
    it "takes text up to the first newline" $
      firstLine "hello\nworld" `shouldBe` "hello"
    it "passes single-line text through unchanged" $
      firstLine "single line" `shouldBe` "single line"

  describe "truncateSummary" $ do
    it "leaves short text untouched" $
      truncateSummary 70 "short" `shouldBe` "short"
    it "cuts at 70 code points on ASCII" $
      T.length (truncateSummary 70 (T.replicate 200 "x")) `shouldBe` 70
    it "cuts at 70 code points on Cyrillic (no byte-level corruption)" $ do
      let cyr = T.replicate 200 "Ы"
          cut = truncateSummary 70 cyr
      T.length cut `shouldBe` 70
      cut `shouldBe` T.replicate 70 "Ы"

  describe "summaryOf" $ do
    let baseTask =
          Task
            { taskId = "tid"
            , taskFrom = "a"
            , taskTo = "b"
            , taskKind = KindFreetext
            , taskUrl = Nothing
            , taskDescription = Nothing
            , taskExpected = Nothing
            , taskStatus = TSPending
            , taskCreatedAt = Timestamp sampleTime
            , taskClaimedAt = Nothing
            , taskCompletedAt = Nothing
            }
    it "takes first line of description, truncated to 70" $ do
      let t =
            baseTask
              { taskDescription =
                  Just (T.replicate 100 "x" <> "\nsecond line")
              }
      T.length (summaryOf t) `shouldBe` 70
      T.any (== '\n') (summaryOf t) `shouldBe` False
    it "falls back to url when description is Nothing" $
      summaryOf (baseTask {taskUrl = Just "poreus://x/y?a=1"})
        `shouldBe` "poreus://x/y?a=1"
    it "falls back to url when description is empty" $
      summaryOf (baseTask {taskDescription = Just "", taskUrl = Just "u"})
        `shouldBe` "u"
    it "returns empty string when both are missing" $
      summaryOf baseTask `shouldBe` ""
    it "preserves Cyrillic content" $ do
      let t = baseTask {taskDescription = Just "Внеси изменения в /etc/nixos"}
      summaryOf t `shouldBe` "Внеси изменения в /etc/nixos"

  describe "toHistoryRow" $ do
    let mk tFrom tTo =
          Task
            { taskId = "tid-1"
            , taskFrom = tFrom
            , taskTo = tTo
            , taskKind = KindFreetext
            , taskUrl = Nothing
            , taskDescription = Just "hello"
            , taskExpected = Nothing
            , taskStatus = TSPending
            , taskCreatedAt = Timestamp sampleTime
            , taskClaimedAt = Nothing
            , taskCompletedAt = Nothing
            }
    it "marks outgoing as '->' with peer = to" $ do
      let r = toHistoryRow "me" (mk "me" "you")
      hrDir r `shouldBe` "->"
      hrPeer r `shouldBe` "you"
    it "marks incoming as '<-' with peer = from" $ do
      let r = toHistoryRow "me" (mk "you" "me")
      hrDir r `shouldBe` "<-"
      hrPeer r `shouldBe` "you"
    it "copies id, kind, status and formats when" $ do
      let r = toHistoryRow "me" (mk "me" "you")
      hrId r `shouldBe` "tid-1"
      hrKind r `shouldBe` KindFreetext
      hrStatus r `shouldBe` TSPending
      hrWhen r `shouldBe` "04-24 08:14"

  describe "formatHistoryTable" $ do
    it "prints 'No messages.' for empty input" $
      formatHistoryTable [] `shouldBe` "No messages.\n"
    it "has header + separator row before data rows" $ do
      let r = sampleRow
          out = formatHistoryTable [r]
          ls = T.lines out
      length ls `shouldBe` 3
      head ls `shouldBe` "| When | Dir | Peer | Kind | Status | Summary |"
      ls !! 1 `shouldBe` "|---|---|---|---|---|---|"
    it "puts dir, peer, kind, status, summary in the data row" $ do
      let r = sampleRow
          out = formatHistoryTable [r]
          dataRow = T.lines out !! 2
      dataRow `shouldBe`
        "| 04-24 08:14 | <- | claude-config | freetext | pending | hello |"
    it "escapes pipe characters inside the summary" $ do
      let r = sampleRow {hrSummary = "a|b|c"}
          out = formatHistoryTable [r]
          dataRow = T.lines out !! 2
      dataRow `shouldSatisfy` T.isInfixOf "a\\|b\\|c"
    it "renders Cyrillic summaries intact" $ do
      let cyr = "Внеси изменения в /etc/nixos"
          r = sampleRow {hrSummary = cyr}
          out = formatHistoryTable [r]
      out `shouldSatisfy` T.isInfixOf cyr

  describe "HistoryRow ToJSON shape" $ do
    it "emits exactly the documented keys" $
      case A.toJSON sampleRow of
        A.Object o ->
          listSort (KM.keys o)
            `shouldBe` listSort
              [ "created_at"
              , "dir"
              , "id"
              , "kind"
              , "peer"
              , "status"
              , "summary"
              , "when"
              ]
        _ -> expectationFailure "expected JSON object"
    it "preserves the raw RFC3339 created_at" $
      case A.toJSON sampleRow of
        A.Object o ->
          KM.lookup "created_at" o
            `shouldBe` Just (A.String "2026-04-24T08:14:06Z")
        _ -> expectationFailure "expected JSON object"

  describe "historyTasks (DB)" $ do
    it "returns tasks where alias is either sender or receiver, DESC" $ do
      let t0 = tsClock emptyTestState
          t1 = addUTCTime 10 t0
          t2 = addUTCTime 20 t0
      (tasks, _) <- withMemoryDB emptyTestState $ \c -> do
        _ <- registerAgent c "me" "/me" (Timestamp t0)
        _ <- registerAgent c "peerA" "/a" (Timestamp t0)
        _ <- registerAgent c "peerB" "/b" (Timestamp t0)
        _ <-
          sendTask
            c
            "me"
            "tid-0"
            (Timestamp t0)
            (SendInput "peerA" KindFreetext Nothing (Just "to A") Nothing)
        _ <-
          sendTask
            c
            "peerB"
            "tid-1"
            (Timestamp t1)
            (SendInput "me" KindFreetext Nothing (Just "from B") Nothing)
        _ <-
          sendTask
            c
            "peerA"
            "tid-2"
            (Timestamp t2)
            (SendInput "peerB" KindFreetext Nothing (Just "not me") Nothing)
        historyTasks c "me" 10
      map taskId tasks `shouldBe` ["tid-1", "tid-0"]

    it "honours the row limit" $ do
      let t0 = tsClock emptyTestState
      (tasks, _) <- withMemoryDB emptyTestState $ \c -> do
        _ <- registerAgent c "me" "/me" (Timestamp t0)
        _ <- registerAgent c "peer" "/p" (Timestamp t0)
        mapM_
          (\i ->
            sendTask
              c
              "me"
              (TaskId ("t-" <> T.pack (show (i :: Int))))
              (Timestamp (addUTCTime (fromIntegral i) t0))
              (SendInput "peer" KindFreetext Nothing (Just "x") Nothing))
          [0 .. 4]
        historyTasks c "me" 3
      length tasks `shouldBe` 3

sampleRow :: HistoryRow
sampleRow =
  HistoryRow
    { hrId = "20260424-081406-claude-config-c57d"
    , hrWhen = "04-24 08:14"
    , hrDir = "<-"
    , hrPeer = "claude-config"
    , hrKind = KindFreetext
    , hrStatus = TSPending
    , hrSummary = "hello"
    , hrCreatedAt = Timestamp sampleTime
    }

listSort :: Ord a => [a] -> [a]
listSort = foldr insertSorted []
  where
    insertSorted x [] = [x]
    insertSorted x (y : ys)
      | x <= y = x : y : ys
      | otherwise = y : insertSorted x ys
