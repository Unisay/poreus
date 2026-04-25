module Poreus.HistorySpec (spec) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import Data.Time (UTCTime, addUTCTime, defaultTimeLocale, parseTimeOrError)
import Test.Hspec

import Poreus.History
import Poreus.Message
import Poreus.Profile (registerAgent)
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

baseMessage :: Message
baseMessage =
  Message
    { msgId = "tid"
    , msgFrom = "a"
    , msgTo = "b"
    , msgKind = MKRequest
    , msgInReplyTo = Nothing
    , msgPayload = A.object []
    , msgSubscribe = Nothing
    , msgCreatedAt = Timestamp sampleTime
    }

mkRequest :: Alias -> Alias -> Maybe T.Text -> Maybe T.Text -> Message
mkRequest from to mDesc mUrl =
  baseMessage
    { msgFrom = from
    , msgTo = to
    , msgKind = MKRequest
    , msgPayload =
        A.object
          [ "request_kind" A..= ("freetext" :: T.Text)
          , "description" A..= mDesc
          , "url" A..= mUrl
          ]
    }

mkNoticeWithSummary :: Alias -> Alias -> T.Text -> Message
mkNoticeWithSummary from to summary =
  baseMessage
    { msgFrom = from
    , msgTo = to
    , msgKind = MKNotice
    , msgInReplyTo = Just "rid"
    , msgPayload = A.object ["summary" A..= summary]
    }

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
    it "request: takes first line of description, truncated to 70" $ do
      let m =
            mkRequest
              "a"
              "b"
              (Just (T.replicate 100 "x" <> "\nsecond line"))
              Nothing
      T.length (summaryOf m) `shouldBe` 70
      T.any (== '\n') (summaryOf m) `shouldBe` False
    it "request: falls back to url when description is absent" $
      summaryOf (mkRequest "a" "b" Nothing (Just "poreus://x/y?a=1"))
        `shouldBe` "poreus://x/y?a=1"
    it "request: returns empty string when both are missing" $
      summaryOf (mkRequest "a" "b" Nothing Nothing) `shouldBe` ""
    it "notice: takes summary first" $
      summaryOf (mkNoticeWithSummary "a" "b" "all green") `shouldBe` "all green"
    it "notice: falls back to event when summary is absent" $ do
      let m =
            baseMessage
              { msgKind = MKNotice
              , msgPayload = A.object ["event" A..= ("completed" :: T.Text)]
              }
      summaryOf m `shouldBe` "completed"
    it "preserves Cyrillic content" $ do
      let m = mkRequest "a" "b" (Just "Внеси изменения в /etc/nixos") Nothing
      summaryOf m `shouldBe` "Внеси изменения в /etc/nixos"

  describe "toHistoryRow" $ do
    let mk tFrom tTo =
          baseMessage
            { msgFrom = tFrom
            , msgTo = tTo
            , msgPayload = A.object ["description" A..= ("hello" :: T.Text)]
            }
    it "marks outgoing as '->' with peer = to" $ do
      let r = toHistoryRow "me" (mk "me" "you")
      hrDir r `shouldBe` "->"
      hrPeer r `shouldBe` "you"
    it "marks incoming as '<-' with peer = from" $ do
      let r = toHistoryRow "me" (mk "you" "me")
      hrDir r `shouldBe` "<-"
      hrPeer r `shouldBe` "you"
    it "copies id, kind and formats when" $ do
      let r = toHistoryRow "me" (mk "me" "you")
      hrId r `shouldBe` "tid"
      hrKind r `shouldBe` MKRequest
      hrWhen r `shouldBe` "04-24 08:14"

  describe "formatHistoryTable" $ do
    it "prints 'No messages.' for empty input" $
      formatHistoryTable [] `shouldBe` "No messages.\n"
    it "has header + separator row before data rows" $ do
      let r = sampleRow
          out = formatHistoryTable [r]
          ls = T.lines out
      length ls `shouldBe` 3
      head ls `shouldBe` "| When | Dir | Peer | Kind | Summary |"
      ls !! 1 `shouldBe` "|---|---|---|---|---|"
    it "puts dir, peer, kind, summary in the data row" $ do
      let r = sampleRow
          out = formatHistoryTable [r]
          dataRow = T.lines out !! 2
      dataRow
        `shouldBe` "| 04-24 08:14 | <- | claude-config | request | hello |"
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
              , "in_reply_to"
              , "kind"
              , "peer"
              , "summary"
              , "when"
              ]
        _ -> expectationFailure "expected JSON object"

  describe "historyMessages (DB)" $ do
    it "returns messages where alias is either sender or receiver, DESC" $ do
      let t = tsClock emptyTestState
          t1 = addUTCTime 10 t
          t2 = addUTCTime 20 t
          mk1 mid from to ts =
            Message
              { msgId = mid
              , msgFrom = from
              , msgTo = to
              , msgKind = MKRequest
              , msgInReplyTo = Nothing
              , msgPayload = A.object []
              , msgSubscribe = Nothing
              , msgCreatedAt = Timestamp ts
              }
      (msgs, _) <- withMemoryDB emptyTestState $ \c -> do
        _ <- registerAgent c "me" "/me" (Timestamp t)
        _ <- registerAgent c "peerA" "/a" (Timestamp t)
        _ <- registerAgent c "peerB" "/b" (Timestamp t)
        insertMessage c (mk1 "tid-0" "me" "peerA" t)
        insertMessage c (mk1 "tid-1" "peerB" "me" t1)
        insertMessage c (mk1 "tid-2" "peerA" "peerB" t2)
        historyMessages c "me" 10
      map msgId msgs `shouldBe` ["tid-1", "tid-0"]

sampleRow :: HistoryRow
sampleRow =
  HistoryRow
    { hrId = "20260424-081406-claude-config-c57d"
    , hrWhen = "04-24 08:14"
    , hrDir = "<-"
    , hrPeer = "claude-config"
    , hrKind = MKRequest
    , hrInReplyTo = Nothing
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
