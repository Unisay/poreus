module Poreus.DeliverSpec (spec) where

import Data.Maybe (isJust, isNothing)
import Database.SQLite.Simple (Connection)
import Test.Hspec

import Poreus.Deliver
import Poreus.Name (claimName, mailboxesOf, releaseName)
import Poreus.Post (Sender (..), postNotify, postRequest)
import Poreus.Session (endSession, ensureSession)
import Poreus.TestM
import Poreus.Types

alice, bob, carol :: SessionAddress
alice = SessionAddress "s-alice"
bob = SessionAddress "s-bob"
carol = SessionAddress "s-carol"

nixos :: Mailbox
nixos = MailboxRole (AgentName "nixos")

setup :: Connection -> TestIOM ()
setup c = do
  _ <- ensureSession c alice "/ws/alice" Nothing Nothing
  _ <- ensureSession c bob "/ws/bob" Nothing Nothing
  _ <- claimName c bob "nixos" False
  setRandomInts [0 ..]
  pure ()

sendReq :: Connection -> TestIOM Message
sendReq c = do
  r <- postRequest c (Sender alice Nothing) "s-bob" "work" Nothing Nothing False
  either (error . show) (pure . fst) r

sendToRole :: Connection -> TestIOM Message
sendToRole c = do
  r <- postRequest c (Sender alice Nothing) "nixos" "work" Nothing Nothing False
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
        d1 <- deliverPending c [MailboxSession bob]
        d2 <- deliverPending c [MailboxSession bob]
        pure (map (msgId . dMessage) d1, [msgId m1, msgId m2], map (msgId . dMessage) d2)
      got1 `shouldBe` expected
      got2 `shouldBe` []

    it "resumes from the cursor: messages arriving later are delivered next (RECV-5)" $ do
      ((got, m2id), _) <- withTestDB initialTestState $ \c -> do
        setup c
        _ <- sendReq c
        _ <- deliverPending c [MailboxSession bob]
        advanceClock 1
        m2 <- sendReq c
        d <- deliverPending c [MailboxSession bob]
        pure (map (msgId . dMessage) d, msgId m2)
      got `shouldBe` [m2id]

    it "attaches the reply duty to requests only (POL-1 carried in-band)" $ do
      (duties, _) <- withTestDB initialTestState $ \c -> do
        setup c
        _ <- sendReq c
        _ <- postNotify c (Sender alice Nothing) "s-bob" (Just "ping") Nothing Nothing False
        d <- deliverPending c [MailboxSession bob]
        pure (map dReplyDuty d)
      length duties `shouldBe` 2
      head duties `shouldSatisfy` isJust
      last duties `shouldSatisfy` isNothing

    it "first-ever delivery yields the full backlog (cursor born at 0)" $ do
      (d, _) <- withTestDB initialTestState $ \c -> do
        _ <- ensureSession c alice "/ws/alice" Nothing Nothing
        setRandomInts [0 ..]
        -- alice never drained anything before this message was sent.
        r <- postRequest c (Sender alice Nothing) "s-alice" "note to self" Nothing Nothing False
        _ <- either (error . show) (pure . fst) r
        deliverPending c [MailboxSession alice]
      length d `shouldBe` 1

  describe "two mailboxes, one stream" $ do
    it "merges the session and role mailboxes back into seq order" $ do
      ((got, expected), _) <- withTestDB initialTestState $ \c -> do
        setup c
        m1 <- sendToRole c
        advanceClock 1
        m2 <- sendReq c
        advanceClock 1
        m3 <- sendToRole c
        boxes <- mailboxesOf c bob
        d <- deliverPending c boxes
        pure (map (msgId . dMessage) d, [msgId m1, msgId m2, msgId m3])
      got `shouldBe` expected

    it "keeps one cursor per mailbox: draining the role leaves the session's mail" $ do
      ((afterRole, afterBoth), _) <- withTestDB initialTestState $ \c -> do
        setup c
        _ <- sendToRole c
        _ <- sendReq c
        a <- deliverPending c [nixos]
        b <- deliverPending c [MailboxSession bob, nixos]
        pure (length a, length b)
      afterRole `shouldBe` 1
      afterBoth `shouldBe` 1

  describe "the role mailbox outlives its holder (RECV-4 without a query flag)" $ do
    it "hands a dead holder's undrained backlog to the successor" $ do
      ((got, expected), _) <- withTestDB initialTestState $ \c -> do
        setup c
        m <- sendToRole c
        -- bob never read it and its session ends; carol takes the role.
        endSession c bob
        _ <- ensureSession c carol "/ws/carol" Nothing Nothing
        _ <- claimName c carol "nixos" False
        boxes <- mailboxesOf c carol
        d <- deliverPending c boxes
        pure (map (msgId . dMessage) d, [msgId m])
      got `shouldBe` expected

    it "does not re-deliver what the previous holder already read" $ do
      (d, _) <- withTestDB initialTestState $ \c -> do
        setup c
        _ <- sendToRole c
        bobBoxes <- mailboxesOf c bob
        _ <- deliverPending c bobBoxes
        endSession c bob
        _ <- ensureSession c carol "/ws/carol" Nothing Nothing
        _ <- claimName c carol "nixos" False
        carolBoxes <- mailboxesOf c carol
        deliverPending c carolBoxes
      d `shouldBe` []

    it "queues for a role nobody holds, and delivers on the next claim" $ do
      ((queued, delivered), _) <- withTestDB initialTestState $ \c -> do
        setup c
        _ <- releaseName c bob
        m <- sendToRole c
        q <- pendingCount c nixos
        _ <- ensureSession c carol "/ws/carol" Nothing Nothing
        _ <- claimName c carol "nixos" False
        boxes <- mailboxesOf c carol
        d <- deliverPending c boxes
        pure (q, map (msgId . dMessage) d == [msgId m])
      queued `shouldBe` 1
      delivered `shouldBe` True

  describe "pendingCount" $ do
    it "counts only what is past the cursor" $ do
      ((before, after), _) <- withTestDB initialTestState $ \c -> do
        setup c
        _ <- sendToRole c
        _ <- sendToRole c
        b <- pendingCount c nixos
        _ <- deliverPending c [nixos]
        a <- pendingCount c nixos
        pure (b, a)
      before `shouldBe` 2
      after `shouldBe` 0

  describe "two concurrent deliverers over one store (server + hook)" $ do
    it "never double-delivers within one mailbox" $ do
      ((d1, d2), _) <- withTestFileDB initialTestState $ \c1 c2 -> do
        _ <- ensureSession c1 alice "/ws/alice" Nothing Nothing
        _ <- ensureSession c1 bob "/ws/bob" Nothing Nothing
        setRandomInts [0 ..]
        r <- postRequest c1 (Sender alice Nothing) "s-bob" "work" Nothing Nothing False
        _ <- either (error . show) (pure . fst) r
        d1 <- deliverPending c1 [MailboxSession bob]
        d2 <- deliverPending c2 [MailboxSession bob]
        pure (d1, d2)
      length d1 `shouldBe` 1
      d2 `shouldBe` []
