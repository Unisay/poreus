module Poreus.WatchSpec (spec) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (addUTCTime)
import Test.Hspec

import Poreus.Message (MessageKind (..), msgFrom, msgInReplyTo, msgKind, msgTo)
import qualified Poreus.Message as Msg
import Poreus.Profile (registerAgent)
import Poreus.Task
  ( CompleteInput (..)
  , SendInput (..)
  , claimTask
  , completeTask
  , newTaskId
  , sendTask
  )
import Poreus.TestM
import Poreus.Time (Timestamp (..))
import Poreus.Types
import Poreus.Watch

spec :: Spec
spec = do
  describe "watchCheck — unified message stream" $ do
    it "returns every unseen message addressed to the alias, then advances cursor" $ do
      let t0 = tsClock emptyTestState
          t1 = addUTCTime 60 t0
          req = newTaskId "bob" (Timestamp t0) "aa01"
      (runs, _) <- withMemoryDB emptyTestState $ \c -> do
        _ <- registerAgent c "bob" "/b" (Timestamp t0)
        _ <- registerAgent c "alice" "/a" (Timestamp t0)
        _ <-
          sendTask
            c
            "bob"
            req
            (Timestamp t0)
            (SendInput "alice" KindFreetext Nothing Nothing Nothing)
        first <- watchCheck c "alice" (Timestamp t1)
        second <- watchCheck c "alice" (Timestamp t1)
        pure (first, second)
      case runs of
        (first, second) -> do
          map Msg.msgId first `shouldBe` [req]
          map msgKind first `shouldBe` [MKRequest]
          second `shouldBe` []

    it "a later-arriving message on a second poll is returned once" $ do
      let t0 = tsClock emptyTestState
          t1 = addUTCTime 60 t0
          t2 = addUTCTime 120 t0
          t3 = addUTCTime 180 t0
          r1 = newTaskId "bob" (Timestamp t0) "0001"
          r2 = newTaskId "bob" (Timestamp t2) "0002"
      (runs, _) <- withMemoryDB emptyTestState $ \c -> do
        _ <- registerAgent c "alice" "/a" (Timestamp t0)
        _ <- registerAgent c "bob" "/b" (Timestamp t0)
        _ <-
          sendTask
            c
            "bob"
            r1
            (Timestamp t0)
            (SendInput "alice" KindFreetext Nothing Nothing Nothing)
        a <- watchCheck c "alice" (Timestamp t1)
        _ <-
          sendTask
            c
            "bob"
            r2
            (Timestamp t2)
            (SendInput "alice" KindFreetext Nothing Nothing Nothing)
        b <- watchCheck c "alice" (Timestamp t3)
        c2 <- watchCheck c "alice" (Timestamp t3)
        pure (a, b, c2)
      case runs of
        (first, second, third) -> do
          map Msg.msgId first `shouldBe` [r1]
          map Msg.msgId second `shouldBe` [r2]
          third `shouldBe` []

    it "reply message surfaces in the sender's watch stream" $ do
      let t0 = tsClock emptyTestState
          t1 = addUTCTime 60 t0
          t2 = addUTCTime 120 t0
          req = newTaskId "alice" (Timestamp t0) "beef"
          rep = newTaskId "bob" (Timestamp t2) "cafe"
      (outs, _) <- withMemoryDB emptyTestState $ \c -> do
        _ <- registerAgent c "alice" "/a" (Timestamp t0)
        _ <- registerAgent c "bob" "/b" (Timestamp t0)
        _ <-
          sendTask
            c
            "alice"
            req
            (Timestamp t0)
            (SendInput "bob" KindFreetext Nothing Nothing Nothing)
        Right _ <- claimTask c req (Timestamp t1)
        Right _ <-
          completeTask
            c
            req
            rep
            (Timestamp t2)
            (CompleteInput RSCompleted (Just "done") Nothing)
        -- First poll as alice sees the reply from bob.
        aliceSeen <- watchCheck c "alice" (Timestamp t2)
        -- First poll as bob sees the original request addressed to him.
        bobSeen <- watchCheck c "bob" (Timestamp t2)
        pure (aliceSeen, bobSeen)
      case outs of
        (aliceSeen, bobSeen) -> do
          map msgKind aliceSeen `shouldBe` [MKReply]
          map Msg.msgId aliceSeen `shouldBe` [rep]
          map msgInReplyTo aliceSeen `shouldBe` [Just req]
          map msgFrom aliceSeen `shouldBe` ["bob"]
          map msgTo aliceSeen `shouldBe` ["alice"]
          map msgKind bobSeen `shouldBe` [MKRequest]

  describe "JSON shape" $ do
    it "uses message_id + in_reply_to at the top level (not id)" $ do
      let t0 = tsClock emptyTestState
          req = newTaskId "bob" (Timestamp t0) "0099"
      (encoded, _) <- withMemoryDB emptyTestState $ \c -> do
        _ <- registerAgent c "bob" "/b" (Timestamp t0)
        _ <- registerAgent c "alice" "/a" (Timestamp t0)
        _ <-
          sendTask
            c
            "bob"
            req
            (Timestamp t0)
            (SendInput "alice" KindFreetext Nothing Nothing Nothing)
        msgs <- watchCheck c "alice" (Timestamp t0)
        pure (TE.decodeUtf8 (BL.toStrict (A.encode msgs)))
      encoded `shouldSatisfy` ("\"message_id\"" `T.isInfixOf`)
      encoded `shouldSatisfy` ("\"in_reply_to\"" `T.isInfixOf`)
      -- must NOT start a key `"id":` at the top (would collide with pre-spec shape).
      encoded `shouldNotSatisfy` ("\"id\":" `T.isInfixOf`)
