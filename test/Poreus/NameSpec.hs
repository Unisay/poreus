module Poreus.NameSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection, fromOnly, query_)
import Test.Hspec

import Poreus.Deliver (cursorOf, deliverPending)
import Poreus.Name
import Poreus.Post (Sender (..), postRequest)
import Poreus.Session (ensureSession)
import Poreus.TestM
import Poreus.Types

alice, bob :: SessionAddress
alice = SessionAddress "s-alice"
bob = SessionAddress "s-bob"

-- | Two live sessions, no bindings.
twoSessions :: Connection -> TestIOM ()
twoSessions c = do
  _ <- ensureSession c alice "/ws/alice" Nothing Nothing
  _ <- ensureSession c bob "/ws/bob" Nothing Nothing
  pure ()

spec :: Spec
spec = do
  describe "validateName" $ do
    it "accepts kebab-case names" $ do
      validateName "nixos" `shouldBe` Right (AgentName "nixos")
      validateName "my-repo2" `shouldBe` Right (AgentName "my-repo2")

    it "rejects the session-address prefix" $ do
      errCodeOf (validateName "s-abc") `shouldBe` Just InvalidInput

    it "rejects empty, uppercase, and malformed names" $ do
      errCodeOf (validateName "") `shouldBe` Just InvalidInput
      errCodeOf (validateName "Bad") `shouldBe` Just InvalidInput
      errCodeOf (validateName "a--b") `shouldBe` Just InvalidInput
      errCodeOf (validateName "-a") `shouldBe` Just InvalidInput

  describe "claimName (REG-3)" $ do
    it "claims a free name" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        twoSessions c
        claimName c alice "nixos" False
      r `shouldBe` Right (ClaimOutcome (AgentName "nixos") Nothing Nothing)

    it "is idempotent for the current holder (v0.2 exit 64 dissolves)" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        twoSessions c
        _ <- claimName c alice "nixos" False
        claimName c alice "nixos" False
      r `shouldBe` Right (ClaimOutcome (AgentName "nixos") Nothing Nothing)

    it "refuses a role held by a live session, without handing out its address" $ do
      -- A refusal that names an address is read as an invitation to
      -- use it (ADR-0017, L5).
      (r, _) <- withTestDB initialTestState $ \c -> do
        twoSessions c
        _ <- claimName c alice "nixos" False
        claimName c bob "nixos" False
      case r of
        Left e -> do
          errCode e `shouldBe` NameHeld
          errMessage e `shouldSatisfy` T.isInfixOf "nixos"
          errMessage e `shouldSatisfy` (not . T.isInfixOf "s-alice")
        Right _ -> expectationFailure "expected name-held"

    it "takes over explicitly, reporting the displaced holder (RECV-2)" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        twoSessions c
        _ <- claimName c alice "nixos" False
        claimName c bob "nixos" True
      r `shouldBe` Right (ClaimOutcome (AgentName "nixos") (Just alice) Nothing)

    it "claims from a dead holder without takeover" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        addProc 500 (ProcInfo Nothing "poreus" True 111)
        _ <- ensureSession c alice "/ws/alice" (Just 500) (Just "boot-test")
        _ <- claimName c alice "nixos" False
        addProc 500 (ProcInfo Nothing "poreus" False 111)
        _ <- ensureSession c bob "/ws/bob" Nothing Nothing
        claimName c bob "nixos" False
      r `shouldBe` Right (ClaimOutcome (AgentName "nixos") (Just alice) Nothing)

    it "one name per session: a new claim releases the previous name" $ do
      ((r, oldBinding), _) <- withTestDB initialTestState $ \c -> do
        twoSessions c
        _ <- claimName c alice "nixos" False
        r <- claimName c alice "folios" False
        old <- getName c (AgentName "nixos")
        pure (r, old >>= nameBoundSession)
      r `shouldBe` Right (ClaimOutcome (AgentName "folios") Nothing (Just (AgentName "nixos")))
      oldBinding `shouldBe` Nothing

  describe "releaseName" $ do
    it "releases the binding but keeps the name row (REG-3)" $ do
      ((released, row), _) <- withTestDB initialTestState $ \c -> do
        twoSessions c
        _ <- claimName c alice "nixos" False
        rel <- releaseName c alice
        r <- getName c (AgentName "nixos")
        pure (rel, r)
      released `shouldBe` Just (AgentName "nixos")
      fmap nameBoundSession row `shouldBe` Just Nothing

  describe "retireName (REG-6)" $ do
    it "deletes the role and surfaces the open-request count" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        twoSessions c
        _ <- claimName c bob "nixos" False
        _ <- postRequest c (Sender alice Nothing) "nixos" "please deploy" Nothing Nothing False
        -- bob read it; nothing is queued, so the retire is allowed.
        _ <- deliverPending c [MailboxRole (AgentName "nixos")]
        r <- retireName c "nixos" False
        gone <- getName c (AgentName "nixos")
        case gone of
          Nothing -> pure r
          Just _ -> pure (Left (mkError InternalError "name not deleted"))
      r `shouldBe` Right (RetireOutcome 1 0)

    it "refuses while mail is still queued for the role" $ do
      -- Retiring destroys the mailbox, and the sender is not present
      -- to notice the loss. So the loss has to be somebody's decision.
      ((r, still), _) <- withTestDB initialTestState $ \c -> do
        twoSessions c
        _ <- claimName c bob "nixos" False
        _ <- postRequest c (Sender alice Nothing) "nixos" "please deploy" Nothing Nothing False
        r <- retireName c "nixos" False
        row <- getName c (AgentName "nixos")
        pure (r, row)
      errCodeOf r `shouldBe` Just InvalidInput
      fmap nameName still `shouldBe` Just (AgentName "nixos")

    it "force retires and reports what it discarded" $ do
      ((r, gone, left), _) <- withTestDB initialTestState $ \c -> do
        twoSessions c
        _ <- claimName c bob "nixos" False
        _ <- postRequest c (Sender alice Nothing) "nixos" "a" Nothing Nothing False
        _ <- postRequest c (Sender alice Nothing) "nixos" "b" Nothing Nothing False
        r <- retireName c "nixos" True
        row <- getName c (AgentName "nixos")
        cur <- cursorOf c (MailboxRole (AgentName "nixos"))
        pure (r, row, cur)
      r `shouldBe` Right (RetireOutcome 2 2)
      gone `shouldBe` Nothing
      left `shouldBe` 0

    it "keeps already-delivered history when forcing" $ do
      (remaining, _) <- withTestDB initialTestState $ \c -> do
        twoSessions c
        _ <- claimName c bob "nixos" False
        _ <- postRequest c (Sender alice Nothing) "nixos" "read" Nothing Nothing False
        _ <- deliverPending c [MailboxRole (AgentName "nixos")]
        _ <- postRequest c (Sender alice Nothing) "nixos" "unread" Nothing Nothing False
        _ <- retireName c "nixos" True
        liftIO (query_ c "SELECT COUNT(*) FROM messages")
      map fromOnly remaining `shouldBe` [1 :: Int]

    it "errors on an unknown name" $ do
      (r, _) <- withTestDB initialTestState $ \c -> retireName c "ghost" False
      errCodeOf r `shouldBe` Just UnknownAgent

  describe "resolveRole (SEND-5)" $ do
    it "rejects a never-claimed name with unknown-recipient" $ do
      (r, _) <- withTestDB initialTestState $ \c -> resolveRole c (AgentName "ghost") False
      errCodeOf r `shouldBe` Just UnknownRecipient

    it "creates the role when the sender asked for it" $ do
      ((r, row), _) <- withTestDB initialTestState $ \c -> do
        x <- resolveRole c (AgentName "future") True
        row <- getName c (AgentName "future")
        pure (x, row)
      fmap fst r `shouldBe` Right (MailboxRole (AgentName "future"))
      fmap (map warnCode . snd) r `shouldBe` Right ["role-created"]
      fmap nameName row `shouldBe` Just (AgentName "future")

    it "queues for a released role and warns that nobody holds it" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        twoSessions c
        _ <- claimName c alice "nixos" False
        _ <- releaseName c alice
        resolveRole c (AgentName "nixos") False
      fmap fst r `shouldBe` Right (MailboxRole (AgentName "nixos"))
      fmap (map warnCode . snd) r `shouldBe` Right ["role-unheld"]

    it "resolves to the role's mailbox regardless of which session holds it" $ do
      ((held, dead), _) <- withTestDB initialTestState $ \c -> do
        addProc 500 (ProcInfo Nothing "poreus" True 111)
        _ <- ensureSession c alice "/ws/alice" (Just 500) (Just "boot-test")
        _ <- claimName c alice "nixos" False
        a <- resolveRole c (AgentName "nixos") False
        addProc 500 (ProcInfo Nothing "poreus" False 111)
        b <- resolveRole c (AgentName "nixos") False
        pure (a, b)
      fmap fst held `shouldBe` Right (MailboxRole (AgentName "nixos"))
      fmap fst dead `shouldBe` Right (MailboxRole (AgentName "nixos"))

  describe "mailboxesOf" $ do
    it "is the session alone when it holds no role" $ do
      (boxes, _) <- withTestDB initialTestState $ \c -> do
        twoSessions c
        mailboxesOf c alice
      boxes `shouldBe` [MailboxSession alice]

    it "adds the role's mailbox once claimed, session first" $ do
      (boxes, _) <- withTestDB initialTestState $ \c -> do
        twoSessions c
        _ <- claimName c alice "nixos" False
        mailboxesOf c alice
      boxes `shouldBe` [MailboxSession alice, MailboxRole (AgentName "nixos")]

  describe "suggestRoleName (role nudge)" $ do
    let repoFixture :: Connection -> TestIOM ()
        repoFixture c = do
          addDir "/ws/alice/.git"
          _ <- ensureSession c alice "/ws/alice" Nothing Nothing
          pure ()

    it "suggests the workspace-derived name for a nameless session in a git repo" $ do
      (s, _) <- withTestDB initialTestState $ \c -> do
        repoFixture c
        suggestRoleName c alice "/ws/alice"
      s `shouldBe` Just (AgentName "alice")

    it "prefers the .poreus/alias override" $ do
      (s, _) <- withTestDB initialTestState $ \c -> do
        repoFixture c
        addFile "/ws/alice/.poreus/alias" "front-desk\n"
        suggestRoleName c alice "/ws/alice"
      s `shouldBe` Just (AgentName "front-desk")

    it "is silent once the session holds a name" $ do
      (s, _) <- withTestDB initialTestState $ \c -> do
        repoFixture c
        _ <- claimName c alice "alice" False
        suggestRoleName c alice "/ws/alice"
      s `shouldBe` Nothing

    it "is silent when another live session holds the role (parallel topic sessions)" $ do
      (s, _) <- withTestDB initialTestState $ \c -> do
        repoFixture c
        _ <- ensureSession c bob "/ws/alice" Nothing Nothing
        _ <- claimName c bob "alice" False
        suggestRoleName c alice "/ws/alice"
      s `shouldBe` Nothing

    it "suggests again when the previous holder is dead (the post-wipe / crash case)" $ do
      (s, _) <- withTestDB initialTestState $ \c -> do
        addDir "/ws/alice/.git"
        addProc 500 (ProcInfo Nothing "poreus" True 111)
        _ <- ensureSession c bob "/ws/alice" (Just 500) (Just "boot-test")
        _ <- claimName c bob "alice" False
        addProc 500 (ProcInfo Nothing "poreus" False 111)
        _ <- ensureSession c alice "/ws/alice" Nothing Nothing
        suggestRoleName c alice "/ws/alice"
      s `shouldBe` Just (AgentName "alice")

    it "is silent outside a git repository (no junk names from /tmp)" $ do
      (s, _) <- withTestDB initialTestState $ \c -> do
        _ <- ensureSession c alice "/tmp/scratch" Nothing Nothing
        suggestRoleName c alice "/tmp/scratch"
      s `shouldBe` Nothing

errCodeOf :: Either PoreusError a -> Maybe ErrorCode
errCodeOf = either (Just . errCode) (const Nothing)
