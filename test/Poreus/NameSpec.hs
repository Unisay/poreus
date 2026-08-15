module Poreus.NameSpec (spec) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection)
import Test.Hspec

import Poreus.Name
import Poreus.Post (Sender (..), postRequest)
import Poreus.Session (endSession, ensureSession)
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

    it "refuses a name bound to another live session, identifying the holder" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        twoSessions c
        _ <- claimName c alice "nixos" False
        claimName c bob "nixos" False
      case r of
        Left e -> do
          errCode e `shouldBe` NameHeld
          errMessage e `shouldSatisfy` T.isInfixOf "s-alice"
        Right _ -> expectationFailure "expected name-held"

    it "takes over explicitly, reporting the displaced holder (RECV-2)" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        twoSessions c
        _ <- claimName c alice "nixos" False
        claimName c bob "nixos" True
      r `shouldBe` Right (ClaimOutcome (AgentName "nixos") (Just alice) Nothing)

    it "claims from a dead holder without takeover" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        _ <- ensureSession c alice "/ws/alice" Nothing Nothing
        _ <- claimName c alice "nixos" False
        advanceClock 60
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
    it "deletes the name and surfaces the open-request count" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        twoSessions c
        _ <- claimName c bob "nixos" False
        _ <- postRequest c (Sender alice Nothing) "nixos" "please deploy" Nothing Nothing
        r <- retireName c "nixos"
        gone <- getName c (AgentName "nixos")
        case gone of
          Nothing -> pure r
          Just _ -> pure (Left (mkError InternalError "name not deleted"))
      r `shouldBe` Right 1

    it "errors on an unknown name" $ do
      (r, _) <- withTestDB initialTestState $ \c -> retireName c "ghost"
      errCodeOf r `shouldBe` Just UnknownAgent

  describe "resolveName (SEND-5)" $ do
    it "rejects a never-claimed name with unknown-recipient" $ do
      (r, _) <- withTestDB initialTestState $ \c -> resolveName c (AgentName "ghost")
      errCodeOf r `shouldBe` Just UnknownRecipient

    it "rejects a released name with name-unbound (OQ-12: fail fast)" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        twoSessions c
        _ <- claimName c alice "nixos" False
        _ <- releaseName c alice
        resolveName c (AgentName "nixos")
      errCodeOf r `shouldBe` Just NameUnbound

    it "rejects a name bound to a dead session with name-unbound" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        _ <- ensureSession c alice "/ws/alice" Nothing Nothing
        _ <- claimName c alice "nixos" False
        advanceClock 60
        resolveName c (AgentName "nixos")
      errCodeOf r `shouldBe` Just NameUnbound

    it "resolves to the session currently bound" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        twoSessions c
        _ <- claimName c bob "nixos" False
        resolveName c (AgentName "nixos")
      r `shouldBe` Right bob

    it "resolves to the ended holder's successor after re-claim (rebinding never reroutes)" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        twoSessions c
        _ <- claimName c alice "nixos" False
        endSession c alice
        _ <- claimName c bob "nixos" False
        resolveName c (AgentName "nixos")
      r `shouldBe` Right bob

    it "points the sender at a live nameless session in the matching workspace (C-7 hint)" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        _ <- ensureSession c alice "/repos/nixos" Nothing Nothing
        resolveName c (AgentName "nixos")
      case r of
        Left e -> do
          errCode e `shouldBe` UnknownRecipient
          fromMaybe "" (errAction e) `shouldSatisfy` T.isInfixOf "s-alice"
          fromMaybe "" (errAction e) `shouldSatisfy` T.isInfixOf "/repos/nixos"
        Right _ -> expectationFailure "expected unknown-recipient"

    it "adds the same hint to name-unbound after a release" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        _ <- ensureSession c alice "/repos/nixos" Nothing Nothing
        _ <- claimName c alice "nixos" False
        _ <- releaseName c alice
        resolveName c (AgentName "nixos")
      case r of
        Left e -> do
          errCode e `shouldBe` NameUnbound
          fromMaybe "" (errAction e) `shouldSatisfy` T.isInfixOf "s-alice"
        Right _ -> expectationFailure "expected name-unbound"

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
        _ <- ensureSession c bob "/ws/alice" Nothing Nothing
        _ <- claimName c bob "alice" False
        advanceClock 60
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
