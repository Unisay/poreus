module Poreus.InboxSpec (spec) where

import qualified Data.Aeson as A
import qualified Data.Text as T
import Data.Time (addUTCTime)
import Test.Hspec

import Poreus.Inbox
import Poreus.Message
import Poreus.Profile (registerAgent)
import Poreus.TestM
import Poreus.Time (Timestamp (..))
import Poreus.Types

mkRequest :: Timestamp -> TaskId -> Alias -> Alias -> Message
mkRequest ts mid from to =
  Message
    { msgId = mid
    , msgFrom = from
    , msgTo = to
    , msgKind = MKRequest
    , msgInReplyTo = Nothing
    , msgPayload =
        A.object
          [ "request_kind" A..= ("freetext" :: T.Text)
          , "description" A..= ("hello" :: T.Text)
          ]
    , msgSubscribe = Nothing
    , msgCreatedAt = ts
    }

mkNotice :: Timestamp -> TaskId -> Alias -> Alias -> TaskId -> T.Text -> Message
mkNotice ts mid from to inReplyTo summary =
  Message
    { msgId = mid
    , msgFrom = from
    , msgTo = to
    , msgKind = MKNotice
    , msgInReplyTo = Just inReplyTo
    , msgPayload = A.object ["summary" A..= summary]
    , msgSubscribe = Nothing
    , msgCreatedAt = ts
    }

spec :: Spec
spec = do
  describe "inboxStreamTick — cursor-driven stream" $ do
    it "returns every unseen message addressed to the alias, then advances cursor" $ do
      let t0 = tsClock emptyTestState
          t1 = addUTCTime 60 t0
      (runs, _) <- withMemoryDB emptyTestState $ \c -> do
        _ <- registerAgent c "bob" "/b" (Timestamp t0)
        _ <- registerAgent c "alice" "/a" (Timestamp t0)
        insertMessage c (mkRequest (Timestamp t0) "rid" "bob" "alice")
        first <- inboxStreamTick c "alice" (Timestamp t1)
        second <- inboxStreamTick c "alice" (Timestamp t1)
        pure (first, second)
      case runs of
        (first, second) -> do
          map msgId first `shouldBe` ["rid"]
          map msgKind first `shouldBe` [MKRequest]
          second `shouldBe` []

    it "later-arriving message on a second poll is returned once" $ do
      let t0 = tsClock emptyTestState
          t1 = addUTCTime 60 t0
          t2 = addUTCTime 120 t0
          t3 = addUTCTime 180 t0
      (runs, _) <- withMemoryDB emptyTestState $ \c -> do
        _ <- registerAgent c "alice" "/a" (Timestamp t0)
        _ <- registerAgent c "bob" "/b" (Timestamp t0)
        insertMessage c (mkRequest (Timestamp t0) "r1" "bob" "alice")
        a <- inboxStreamTick c "alice" (Timestamp t1)
        insertMessage c (mkRequest (Timestamp t2) "r2" "bob" "alice")
        b <- inboxStreamTick c "alice" (Timestamp t3)
        c2 <- inboxStreamTick c "alice" (Timestamp t3)
        pure (a, b, c2)
      case runs of
        (first, second, third) -> do
          map msgId first `shouldBe` ["r1"]
          map msgId second `shouldBe` ["r2"]
          third `shouldBe` []

    it "delivers a message that arrives within the same second the cursor advances to" $ do
      -- Regression test for the same-second cursor-advance race fixed by
      -- moving formatUtc to millisecond precision. With second-precision
      -- timestamps, a tick at wall-clock 12:34:56.100 finds an empty
      -- inbox, advances the cursor to "12:34:56", and a message inserted
      -- 700ms later with created_at = "12:34:56" is permanently lost
      -- (`WHERE created_at > "12:34:56"` skips it).
      --
      -- With ms precision, the tick stamps the cursor "...:56.100Z" and
      -- the later insert at "...:56.800Z" is still strictly greater.
      let t0    = tsClock emptyTestState
          tick1 = addUTCTime 0.1   t0  -- :00.100
          msgT  = addUTCTime 0.8   t0  -- :00.800
          tick2 = addUTCTime 5.0   t0  -- next tick, 5s later
      (delivered, _) <- withMemoryDB emptyTestState $ \c -> do
        _ <- registerAgent c "alice" "/a" (Timestamp t0)
        _ <- registerAgent c "bob"   "/b" (Timestamp t0)
        first <- inboxStreamTick c "alice" (Timestamp tick1)   -- empty, advances cursor
        insertMessage c (mkRequest (Timestamp msgT) "rid" "bob" "alice")
        second <- inboxStreamTick c "alice" (Timestamp tick2)
        pure (first, second)
      case delivered of
        (first, second) -> do
          first  `shouldBe` []
          map msgId second `shouldBe` ["rid"]

    it "notice surfaces in the original sender's stream" $ do
      let t0 = tsClock emptyTestState
          t2 = addUTCTime 120 t0
      (outs, _) <- withMemoryDB emptyTestState $ \c -> do
        _ <- registerAgent c "alice" "/a" (Timestamp t0)
        _ <- registerAgent c "bob" "/b" (Timestamp t0)
        insertMessage c (mkRequest (Timestamp t0) "rid" "alice" "bob")
        insertMessage
          c
          (mkNotice (Timestamp t2) "nid" "bob" "alice" "rid" "done")
        aliceSeen <- inboxStreamTick c "alice" (Timestamp t2)
        pure aliceSeen
      map msgKind outs `shouldBe` [MKNotice]
      map msgInReplyTo outs `shouldBe` [Just "rid"]
      map msgFrom outs `shouldBe` ["bob"]

  describe "inboxSnapshot — side-effect free SELECT" $ do
    it "filters by kind" $ do
      let t0 = tsClock emptyTestState
      (results, _) <- withMemoryDB emptyTestState $ \c -> do
        _ <- registerAgent c "alice" "/a" (Timestamp t0)
        _ <- registerAgent c "bob" "/b" (Timestamp t0)
        insertMessage c (mkRequest (Timestamp t0) "rid" "bob" "alice")
        insertMessage
          c
          (mkNotice (Timestamp t0) "nid" "bob" "alice" "rid" "done")
        reqs <-
          inboxSnapshot c "alice" (noFilters {ifKind = Just MKRequest})
        notices <-
          inboxSnapshot c "alice" (noFilters {ifKind = Just MKNotice})
        pure (reqs, notices)
      case results of
        (reqs, notices) -> do
          map msgId reqs `shouldBe` ["rid"]
          map msgId notices `shouldBe` ["nid"]

    it "filters by in_reply_to" $ do
      let t0 = tsClock emptyTestState
      (msgs, _) <- withMemoryDB emptyTestState $ \c -> do
        _ <- registerAgent c "alice" "/a" (Timestamp t0)
        _ <- registerAgent c "bob" "/b" (Timestamp t0)
        insertMessage c (mkRequest (Timestamp t0) "r1" "bob" "alice")
        insertMessage c (mkRequest (Timestamp t0) "r2" "bob" "alice")
        insertMessage
          c
          (mkNotice (Timestamp t0) "n1" "bob" "alice" "r1" "x")
        inboxSnapshot c "alice" (noFilters {ifInReplyTo = Just "r1"})
      map msgId msgs `shouldBe` ["n1"]

    it "does not advance the follow cursor (idempotent)" $ do
      let t0 = tsClock emptyTestState
          t1 = addUTCTime 60 t0
      (afterStream, _) <- withMemoryDB emptyTestState $ \c -> do
        _ <- registerAgent c "alice" "/a" (Timestamp t0)
        _ <- registerAgent c "bob" "/b" (Timestamp t0)
        insertMessage c (mkRequest (Timestamp t0) "rid" "bob" "alice")
        _ <- inboxSnapshot c "alice" noFilters       -- snapshot first
        _ <- inboxSnapshot c "alice" noFilters       -- and again
        inboxStreamTick c "alice" (Timestamp t1)     -- stream still sees it
      map msgId afterStream `shouldBe` ["rid"]

  describe "formatInboxLine — Monitor stdout format" $ do
    it "renders a freetext request" $ do
      let m = mkRequest (Timestamp (tsClock emptyTestState)) "rid" "bob" "alice"
      formatInboxLine m
        `shouldBe` "[POREUS:IN] bob request (freetext): hello"

    it "renders an rpc request from payload.url" $ do
      let m =
            (mkRequest (Timestamp (tsClock emptyTestState)) "rid" "bob" "alice")
              { msgPayload =
                  A.object
                    [ "request_kind" A..= ("rpc" :: T.Text)
                    , "url" A..= ("poreus://nixos/deploy-foo/abc" :: T.Text)
                    ]
              }
      formatInboxLine m
        `shouldBe` "[POREUS:IN] bob request (rpc): poreus://nixos/deploy-foo/abc"

    it "renders a notice with summary and in_reply_to" $ do
      let m =
            mkNotice
              (Timestamp (tsClock emptyTestState))
              "nid"
              "bob"
              "alice"
              "rid"
              "all green"
      formatInboxLine m
        `shouldBe` "[POREUS:IN] bob notice [in-reply-to: rid]: all green"

    it "renders a notice without in_reply_to" $ do
      let m =
            (mkNotice
              (Timestamp (tsClock emptyTestState))
              "nid"
              "bob"
              "alice"
              "rid"
              "all green")
              { msgInReplyTo = Nothing
              , msgPayload = A.object ["event" A..= ("started" :: T.Text)]
              }
      formatInboxLine m
        `shouldBe` "[POREUS:IN] bob notice: started"
