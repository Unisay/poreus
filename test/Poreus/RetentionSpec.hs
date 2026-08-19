module Poreus.RetentionSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Database.SQLite.Simple (Connection, Only (..), Query, query_)
import Test.Hspec

import Poreus.Deliver (deliverPending)
import Poreus.Name (NameRow (..), claimName, getName)
import Poreus.Post (Sender (..), postRequest)
import Poreus.Retention
import Poreus.Session (SessionRow (..), endSession, ensureSession, getSession)
import Poreus.TestM
import Poreus.Types

alice, bob :: SessionAddress
alice = SessionAddress "s-alice"
bob = SessionAddress "s-bob"

spec :: Spec
spec = do
  describe "retentionDays" $ do
    it "defaults to 30 days" $ do
      evalTestM retentionDays emptyTestState `shouldBe` 30

    it "honours a numeric $POREUS_RETENTION_DAYS" $ do
      let st = execTestM (setEnv "POREUS_RETENTION_DAYS" "7") emptyTestState
      evalTestM retentionDays st `shouldBe` 7

    it "ignores garbage and non-positive values" $ do
      let bad = execTestM (setEnv "POREUS_RETENTION_DAYS" "soon") emptyTestState
          zero = execTestM (setEnv "POREUS_RETENTION_DAYS" "0") emptyTestState
      evalTestM retentionDays bad `shouldBe` 30
      evalTestM retentionDays zero `shouldBe` 30

  describe "sweep (MAINT-1)" $ do
    it "deletes messages older than the window and keeps newer ones" $ do
      ((result, remaining), _) <- withTestDB initialTestState $ \c -> do
        _ <- ensureSession c alice "/ws/a" Nothing Nothing
        _ <- ensureSession c bob "/ws/b" Nothing Nothing
        setRandomInts [0 ..]
        _ <- postRequest c (Sender alice Nothing) "s-bob" "old" Nothing Nothing False
        advanceClock (40 * 86400)
        _ <- ensureSession c alice "/ws/a" Nothing Nothing
        _ <- ensureSession c bob "/ws/b" Nothing Nothing
        _ <- postRequest c (Sender alice Nothing) "s-bob" "new" Nothing Nothing False
        r <- sweep c 30
        rows <- countRows c "SELECT COUNT(*) FROM messages"
        pure (r, rows)
      swMessagesDeleted result `shouldBe` 1
      remaining `shouldBe` 1

    it "expires long-ended sessions; their orphaned cursors go too, names survive" $ do
      ((result, sess, nameRow, cursorCount), _) <- withTestDB initialTestState $ \c -> do
        _ <- ensureSession c alice "/ws/a" Nothing Nothing
        _ <- claimName c alice "nixos" False
        endSession c alice
        advanceClock (40 * 86400)
        r <- sweep c 30
        s <- getSession c alice
        n <- getName c (AgentName "nixos")
        cur <- countRows c "SELECT COUNT(*) FROM cursors"
        pure (r, s, n, cur)
      swSessionsDeleted result `shouldBe` 1
      sess `shouldBe` Nothing
      fmap nameName nameRow `shouldBe` Just (AgentName "nixos")
      fmap nameBoundSession nameRow `shouldBe` Just Nothing
      -- `cursors` lost its foreign key when mailboxes moved to roles
      -- (a role mailbox has no sessions row to cascade from), so the
      -- sweep deletes orphans explicitly (ADR-0017 §4).
      cursorCount `shouldBe` 0
      swCursorsDeleted result `shouldBe` 1

    it "keeps live sessions and their traffic" $ do
      ((result, sess), _) <- withTestDB initialTestState $ \c -> do
        _ <- ensureSession c alice "/ws/a" Nothing Nothing
        r <- sweep c 30
        s <- getSession c alice
        pure (r, s)
      swSessionsDeleted result `shouldBe` 0
      fmap sessAddress sess `shouldBe` Just alice

    it "keeps a live role's cursor even with no messages left" $ do
      (cursorCount, _) <- withTestDB initialTestState $ \c -> do
        _ <- ensureSession c alice "/ws/a" Nothing Nothing
        _ <- claimName c alice "nixos" False
        _ <- deliverPending c [MailboxRole (AgentName "nixos")]
        _ <- sweep c 30
        countRows c "SELECT COUNT(*) FROM cursors WHERE mailbox = \'nixos\'"
      cursorCount `shouldBe` 1

  describe "sweepIfDue (the sweep moved to the hook path)" $ do
    it "runs the first time and records that it ran" $ do
      ((r, stamped), _) <- withTestDB initialTestState $ \c -> do
        _ <- ensureSession c alice "/ws/a" Nothing Nothing
        x <- sweepIfDue c 30
        n <- countRows c "SELECT COUNT(*) FROM maintenance WHERE key = \'last_sweep\'"
        pure (x, n)
      r `shouldSatisfy` (/= Nothing)
      stamped `shouldBe` 1

    it "does nothing again within the hour" $ do
      -- The hook fires on every prompt. Without this guard, so would
      -- the sweep.
      (r, _) <- withTestDB initialTestState $ \c -> do
        _ <- ensureSession c alice "/ws/a" Nothing Nothing
        _ <- sweepIfDue c 30
        advanceClock 1800
        sweepIfDue c 30
      r `shouldBe` Nothing

    it "runs again once the interval has passed" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        _ <- ensureSession c alice "/ws/a" Nothing Nothing
        _ <- sweepIfDue c 30
        advanceClock (sweepIntervalSeconds + 1)
        sweepIfDue c 30
      r `shouldSatisfy` (/= Nothing)

countRows :: Connection -> Query -> TestIOM Int
countRows c sql = do
  rows <- liftIO (query_ c sql)
  pure $ case rows of
    (Only n : _) -> n
    [] -> 0
