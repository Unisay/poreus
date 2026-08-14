module Poreus.QuerySpec (spec) where

import Control.Monad (void)
import qualified Control.Monad.State.Strict as MS
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.SQLite.Simple (Connection)
import Test.Hspec

import Poreus.Name (claimName, releaseName)
import Poreus.Post (Sender (..), postNotify, postReply, postRequest)
import Poreus.Query
import Poreus.Session (endSession, ensureSession)
import Poreus.TestM
import Poreus.Time (Timestamp (..))
import Poreus.Types

alice, bob, carol :: SessionAddress
alice = SessionAddress "s-alice"
bob = SessionAddress "s-bob"
carol = SessionAddress "s-carol"

setup :: Connection -> TestIOM ()
setup c = do
  _ <- ensureSession c alice "/ws/alice" Nothing Nothing
  _ <- ensureSession c bob "/ws/bob" Nothing Nothing
  _ <- ensureSession c carol "/ws/carol" Nothing Nothing
  _ <- claimName c bob "nixos" False
  setRandomInts [0 ..]
  pure ()

reqTo :: Connection -> Sender -> Text -> TestIOM Message
reqTo c s to = do
  r <- postRequest c s to "do it" Nothing Nothing
  case r of
    Right (m, _) -> pure m
    Left e -> error (show e)

spec :: Spec
spec = do
  describe "scope inbox (RECV-3)" $ do
    it "returns messages to me, chronological, without touching any cursor" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        setup c
        m1 <- reqTo c (Sender alice Nothing) "nixos"
        advanceClock 1
        m2 <- reqTo c (Sender carol Nothing) "s-bob"
        _ <- reqTo c (Sender bob Nothing) "s-alice"
        r <- runQuery c bob (Just (AgentName "nixos")) ScopeInbox noQueryFilters
        pure (fmap (map msgId . qrMessages) r, m1, m2)
      case r of
        (Right ids, m1, m2) -> ids `shouldBe` [msgId m1, msgId m2]
        (Left e, _, _) -> expectationFailure (show e)

    it "composes kind, from, and since filters" $ do
      (ids, _) <- withTestDB initialTestState $ \c -> do
        setup c
        _ <- reqTo c (Sender alice Nothing) "s-bob"
        advanceClock 10
        cutoff <- Timestamp <$> currentTimeIOM
        advanceClock 10
        m2 <- reqTo c (Sender alice Nothing) "s-bob"
        _ <- postNotify c (Sender alice Nothing) "s-bob" (Just "ping") Nothing Nothing
        _ <- reqTo c (Sender carol Nothing) "s-bob"
        r <-
          runQuery
            c
            bob
            Nothing
            ScopeInbox
            noQueryFilters
              { qfKind = Just MKRequest
              , qfFrom = Just "s-alice"
              , qfSince = Just cutoff
              }
        pure (fmap (map msgId . qrMessages) r, m2)
      case ids of
        (Right got, m2) -> got `shouldBe` [msgId m2]
        (Left e, _) -> expectationFailure (show e)

    it "matches the from filter against the sender's name annotation" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        setup c
        _ <- claimName c alice "folios" False
        _ <- reqTo c (Sender alice (Just (AgentName "folios"))) "s-bob"
        _ <- reqTo c (Sender carol Nothing) "s-bob"
        runQuery c bob Nothing ScopeInbox noQueryFilters{qfFrom = Just "folios"}
      fmap (map msgFrom . qrMessages) r `shouldBe` Right [alice]

  describe "scope open (RECV-4)" $ do
    it "lists requests with no notice from anyone, and drops replied ones" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        setup c
        m1 <- reqTo c (Sender alice Nothing) "nixos"
        advanceClock 1
        m2 <- reqTo c (Sender carol Nothing) "nixos"
        _ <- postReply c (Sender bob Nothing) (msgId m1) "completed" Nothing Nothing
        r <- runQuery c bob Nothing ScopeOpen noQueryFilters
        pure (fmap (map msgId . qrMessages) r, m2)
      case r of
        (Right ids, m2) -> ids `shouldBe` [msgId m2]
        (Left e, _) -> expectationFailure (show e)

    it "adoption scope surfaces requests stranded on a former holder" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        setup c
        m <- reqTo c (Sender alice Nothing) "nixos"
        -- bob (the holder) dies; carol claims the role.
        endSession c bob
        _ <- claimName c carol "nixos" False
        r <- runQuery c carol (Just (AgentName "nixos")) ScopeOpen noQueryFilters{qfAdoption = True}
        pure (fmap (map msgId . qrMessages) r, m)
      case r of
        (Right ids, m) -> ids `shouldBe` [msgId m]
        (Left e, _) -> expectationFailure (show e)

    it "without adoption, a stranded request stays out of the successor's sweep" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        setup c
        _ <- reqTo c (Sender alice Nothing) "nixos"
        endSession c bob
        _ <- claimName c carol "nixos" False
        runQuery c carol (Just (AgentName "nixos")) ScopeOpen noQueryFilters
      fmap qrMessages r `shouldBe` Right []

    it "an adopted (replied) request drops out of everyone's sweep" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        setup c
        m <- reqTo c (Sender alice Nothing) "nixos"
        endSession c bob
        _ <- claimName c carol "nixos" False
        _ <- postReply c (Sender carol (Just (AgentName "nixos"))) (msgId m) "started" Nothing Nothing
        runQuery c carol (Just (AgentName "nixos")) ScopeOpen noQueryFilters{qfAdoption = True}
      fmap qrMessages r `shouldBe` Right []

  describe "scope history (RECV-6)" $ do
    it "merges sent and received, newest first, default limit 10" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        setup c
        mapM_ (\(_ :: Int) -> advanceClock 1 >> void (reqTo c (Sender alice Nothing) "s-bob")) [1 .. 12]
        advanceClock 1
        _ <- reqTo c (Sender bob Nothing) "s-carol"
        runQuery c bob Nothing ScopeHistory noQueryFilters
      case r of
        Left e -> expectationFailure (show e)
        Right qr -> do
          length (qrMessages qr) `shouldBe` 10
          -- Newest first: the send to carol tops the list.
          map msgTo (take 1 (qrMessages qr)) `shouldBe` [carol]

    it "queries any address, not only mine" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        setup c
        _ <- reqTo c (Sender alice Nothing) "s-bob"
        _ <- reqTo c (Sender carol Nothing) "s-bob"
        runQuery c bob Nothing ScopeHistory noQueryFilters{qfInvolving = Just "s-carol"}
      fmap (map msgFrom . qrMessages) r `shouldBe` Right [carol]

    it "resolves a name in involving via the annotations" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        setup c
        _ <- reqTo c (Sender alice Nothing) "nixos"
        _ <- reqTo c (Sender alice Nothing) "s-carol"
        runQuery c alice Nothing ScopeHistory noQueryFilters{qfInvolving = Just "nixos"}
      fmap (map msgTo . qrMessages) r `shouldBe` Right [bob]

  describe "scope thread (THRD-1/2)" $ do
    it "returns root + replies chronologically with derived status" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        setup c
        m <- reqTo c (Sender alice Nothing) "nixos"
        advanceClock 1
        _ <- postReply c (Sender bob Nothing) (msgId m) "started" Nothing Nothing
        advanceClock 1
        _ <- postReply c (Sender bob Nothing) (msgId m) "completed" (Just "done") Nothing
        runQuery c alice Nothing ScopeThread noQueryFilters{qfThread = Just (msgId m)}
      case r of
        Left e -> expectationFailure (show e)
        Right qr -> do
          map msgKind (qrMessages qr) `shouldBe` [MKRequest, MKNotice, MKNotice]
          fmap thsState (qrThreadStatus qr) `shouldBe` Just "terminal"
          (qrThreadStatus qr >>= thsTerminalEvent) `shouldBe` Just "completed"

    it "reports open with no replies and active with only non-terminal ones" $ do
      ((open, active), _) <- withTestDB initialTestState $ \c -> do
        setup c
        m <- reqTo c (Sender alice Nothing) "nixos"
        o <- runQuery c alice Nothing ScopeThread noQueryFilters{qfThread = Just (msgId m)}
        _ <- postReply c (Sender bob Nothing) (msgId m) "started" Nothing Nothing
        a <- runQuery c alice Nothing ScopeThread noQueryFilters{qfThread = Just (msgId m)}
        pure (o, a)
      fmap (fmap thsState . qrThreadStatus) open `shouldBe` Right (Just "open")
      fmap (fmap thsState . qrThreadStatus) active `shouldBe` Right (Just "active")

    it "errors on an unknown thread id" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        setup c
        runQuery c alice Nothing ScopeThread noQueryFilters{qfThread = Just (MessageId "nope")}
      either (Just . errCode) (const Nothing) r `shouldBe` Just UnknownMessage

    it "requires the thread id" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        setup c
        runQuery c alice Nothing ScopeThread noQueryFilters
      either (Just . errCode) (const Nothing) r `shouldBe` Just InvalidInput

    it "works from either side of the conversation" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        setup c
        m <- reqTo c (Sender alice Nothing) "nixos"
        _ <- postReply c (Sender bob Nothing) (msgId m) "completed" Nothing Nothing
        runQuery c bob (Just (AgentName "nixos")) ScopeThread noQueryFilters{qfThread = Just (msgId m)}
      fmap (length . qrMessages) r `shouldBe` Right 2

    it "unblocked release: released requester still sees the thread until retention" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        setup c
        m <- reqTo c (Sender alice Nothing) "nixos"
        _ <- releaseName c bob
        runQuery c alice Nothing ScopeThread noQueryFilters{qfThread = Just (msgId m)}
      fmap (length . qrMessages) r `shouldBe` Right 1

-- Helper: current fake time inside TestIOM.
currentTimeIOM :: TestIOM UTCTime
currentTimeIOM = MS.gets tsClock
