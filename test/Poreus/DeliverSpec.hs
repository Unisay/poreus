module Poreus.DeliverSpec (spec) where

import Data.Maybe (isJust, isNothing)
import Database.SQLite.Simple (Connection)
import Test.Hspec

import Poreus.Deliver
import Poreus.Post (Sender (..), postNotify, postRequest)
import Poreus.Session (ensureSession)
import Poreus.TestM
import Poreus.Types

alice, bob :: SessionAddress
alice = SessionAddress "s-alice"
bob = SessionAddress "s-bob"

setup :: Connection -> TestIOM ()
setup c = do
  _ <- ensureSession c alice "/ws/alice" Nothing Nothing
  _ <- ensureSession c bob "/ws/bob" Nothing Nothing
  setRandomInts [0 ..]
  pure ()

sendReq :: Connection -> TestIOM Message
sendReq c = do
  r <- postRequest c (Sender alice Nothing) "s-bob" "work" Nothing Nothing
  either (error . show) (pure . fst) r

spec :: Spec
spec = do
  describe "deliverPending (RECV-1)" $ do
    it "delivers everything past the cursor exactly once, in order" $ do
      ((got1, expected, got2), _) <- withTestDB initialTestState $ \c -> do
        setup c
        m1 <- sendReq c
        advanceClock 1
        m2 <- sendReq c
        d1 <- deliverPending c bob
        d2 <- deliverPending c bob
        pure (map (msgId . dMessage) d1, [msgId m1, msgId m2], map (msgId . dMessage) d2)
      got1 `shouldBe` expected
      got2 `shouldBe` []

    it "resumes from the cursor: messages arriving later are delivered next (RECV-5)" $ do
      ((got, m2id), _) <- withTestDB initialTestState $ \c -> do
        setup c
        _ <- sendReq c
        _ <- deliverPending c bob
        advanceClock 1
        m2 <- sendReq c
        d <- deliverPending c bob
        pure (map (msgId . dMessage) d, msgId m2)
      got `shouldBe` [m2id]

    it "attaches the reply duty to requests only (POL-1 carried in-band)" $ do
      (duties, _) <- withTestDB initialTestState $ \c -> do
        setup c
        _ <- sendReq c
        _ <- postNotify c (Sender alice Nothing) "s-bob" (Just "ping") Nothing Nothing
        d <- deliverPending c bob
        pure (map dReplyDuty d)
      length duties `shouldBe` 2
      head duties `shouldSatisfy` isJust
      last duties `shouldSatisfy` isNothing

    it "first-ever delivery yields the full backlog (cursor born at 0)" $ do
      (d, _) <- withTestDB initialTestState $ \c -> do
        _ <- ensureSession c alice "/ws/alice" Nothing Nothing
        setRandomInts [0 ..]
        -- carol never contacted the server before this message was sent...
        r <- postRequest c (Sender alice Nothing) "s-alice" "note to self" Nothing Nothing
        _ <- either (error . show) (pure . fst) r
        deliverPending c alice
      length d `shouldBe` 1

  describe "peekPendingSince (channel path)" $ do
    it "never advances the cursor: a later deliverPending still returns the message" $ do
      ((peeked, expected, delivered), _) <- withTestDB initialTestState $ \c -> do
        setup c
        m <- sendReq c
        p <- peekPendingSince c bob 0
        d <- deliverPending c bob
        pure (map msgId p, [msgId m], map (msgId . dMessage) d)
      peeked `shouldBe` expected
      length delivered `shouldBe` 1

  describe "two concurrent deliverers over one store (server + hook)" $ do
    it "never double-delivers within one mailbox" $ do
      ((d1, d2), _) <- withTestFileDB initialTestState $ \c1 c2 -> do
        _ <- ensureSession c1 alice "/ws/alice" Nothing Nothing
        _ <- ensureSession c1 bob "/ws/bob" Nothing Nothing
        setRandomInts [0 ..]
        r <- postRequest c1 (Sender alice Nothing) "s-bob" "work" Nothing Nothing
        _ <- either (error . show) (pure . fst) r
        d1 <- deliverPending c1 bob
        d2 <- deliverPending c2 bob
        pure (d1, d2)
      length d1 `shouldBe` 1
      d2 `shouldBe` []
