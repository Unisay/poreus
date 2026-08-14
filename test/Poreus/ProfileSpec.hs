module Poreus.ProfileSpec (spec) where

import Database.SQLite.Simple (Connection)
import Test.Hspec

import Poreus.Name (NameRow (..), boundNameOf, claimName, getName)
import Poreus.Profile
import Poreus.Session (ensureSession)
import Poreus.TestM
import Poreus.Types

alice, bob :: SessionAddress
alice = SessionAddress "s-alice"
bob = SessionAddress "s-bob"

deployEp :: EndpointInput
deployEp = EndpointInput "deploy-poreus" "Deploy poreus to the host" AutonomyConfirm (Just "args: {sha}")

statusEp :: EndpointInput
statusEp = EndpointInput "status" "Report service status" AutonomyAuto Nothing

setup :: Connection -> TestIOM ()
setup c = do
  _ <- ensureSession c alice "/ws/alice" Nothing Nothing
  _ <- ensureSession c bob "/ws/bob" Nothing Nothing
  pure ()

spec :: Spec
spec = do
  describe "publishProfile (REG-4)" $ do
    it "claims the name implicitly and stores the profile" $ do
      ((r, row, eps, bound), _) <- withTestDB initialTestState $ \c -> do
        setup c
        r <- publishProfile c alice (Just "nixos") "NixOS host" ["nix", "deploy"] [deployEp, statusEp]
        row <- getName c (AgentName "nixos")
        eps <- endpointsOf c (AgentName "nixos")
        bound <- boundNameOf c alice
        pure (r, row, eps, bound)
      fmap prEndpointCount r `shouldBe` Right 2
      fmap nameSummary row `shouldBe` Just (Just "NixOS host")
      fmap nameTags row `shouldBe` Just ["nix", "deploy"]
      map epVerb eps `shouldBe` ["deploy-poreus", "status"]
      bound `shouldBe` Just (AgentName "nixos")

    it "atomically replaces the endpoint set on re-publish" $ do
      (eps, _) <- withTestDB initialTestState $ \c -> do
        setup c
        _ <- publishProfile c alice (Just "nixos") "v1" [] [deployEp, statusEp]
        _ <- publishProfile c alice (Just "nixos") "v2" [] [statusEp]
        endpointsOf c (AgentName "nixos")
      map epVerb eps `shouldBe` ["status"]

    it "defaults to the session's bound name" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        setup c
        _ <- claimName c alice "nixos" False
        publishProfile c alice Nothing "NixOS host" [] []
      fmap prName r `shouldBe` Right (AgentName "nixos")

    it "requires a name when none is bound" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        setup c
        publishProfile c alice Nothing "x" [] []
      leftCode r `shouldBe` Just InvalidInput

    it "refuses to publish over another live session's name" $ do
      (r, _) <- withTestDB initialTestState $ \c -> do
        setup c
        _ <- claimName c bob "nixos" False
        publishProfile c alice (Just "nixos") "mine now" [] []
      leftCode r `shouldBe` Just NameHeld

  describe "validateEndpoints" $ do
    it "rejects non-kebab verbs" $ do
      let bad = EndpointInput "Deploy_Now" "x" AutonomyAuto Nothing
      leftCode (validateEndpoints [bad]) `shouldBe` Just InvalidInput

    it "rejects duplicate verbs" $ do
      leftCode (validateEndpoints [statusEp, statusEp]) `shouldBe` Just InvalidInput

    it "accepts a lean, valid set" $ do
      validateEndpoints [deployEp, statusEp] `shouldBe` Right ()

leftCode :: Either PoreusError a -> Maybe ErrorCode
leftCode = either (Just . errCode) (const Nothing)
