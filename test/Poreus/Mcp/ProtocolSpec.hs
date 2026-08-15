module Poreus.Mcp.ProtocolSpec (spec) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple (Connection)
import Test.Hspec

import Poreus.Identity (Identity (..), IdentitySource (..), addressFromSessionId)
import Poreus.Mcp.Protocol
import Poreus.Mcp.Tools (McpEnv (..))
import Poreus.TestM

-- | A protocol environment for one fake session over a shared store.
mkEnv :: Connection -> Text -> McpEnv
mkEnv c sid =
  McpEnv
    { envConn = c
    , envIdentity =
        Identity
          { idAddress = addressFromSessionId sid
          , idSessionId = sid
          , idWorkspace = "/ws/" <> sid
          , idSource = SourceEnvOverride
          }
    , envPid = Nothing
    , envBootId = Nothing
    }

rpc :: Int -> Text -> Value -> Value
rpc n method params =
  object ["jsonrpc" .= ("2.0" :: Text), "id" .= n, "method" .= method, "params" .= params]

toolCall :: Int -> Text -> [(Key.Key, Value)] -> Value
toolCall n name args =
  rpc n "tools/call" (object ["name" .= name, "arguments" .= object args])

-- | Path lookup into nested objects.
(.?) :: Value -> Text -> Value
Object o .? k = fromMaybe Null (KM.lookup (Key.fromText k) o)
_ .? _ = Null

infixl 9 .?

asText :: Value -> Maybe Text
asText (String t) = Just t
asText _ = Nothing

spec :: Spec
spec = do
  describe "initialize" $ do
    it "echoes a supported protocol version and describes the server" $ do
      (outs, _) <- withTestDB initialTestState $ \c ->
        handleValue (mkEnv c "alice") $
          rpc 1 "initialize" (object ["protocolVersion" .= ("2025-03-26" :: Text)])
      case outs of
        [r] -> do
          asText (r .? "result" .? "protocolVersion") `shouldBe` Just "2025-03-26"
          asText (r .? "result" .? "serverInfo" .? "name") `shouldBe` Just "poreus"
          (r .? "result" .? "instructions") `shouldSatisfy` \case
            String t -> "reply" `T.isInfixOf` t
            _ -> False
        _ -> expectationFailure ("expected one response, got " <> show outs)

    it "offers its newest version to a client asking for an unknown one" $ do
      (outs, _) <- withTestDB initialTestState $ \c ->
        handleValue (mkEnv c "alice") $
          rpc 1 "initialize" (object ["protocolVersion" .= ("2099-01-01" :: Text)])
      case outs of
        [r] -> asText (r .? "result" .? "protocolVersion") `shouldBe` Just (head supportedVersions)
        _ -> expectationFailure "expected one response"

    it "auto-provisions the session (REG-2: whoami works right after)" $ do
      (outs, _) <- withTestDB initialTestState $ \c -> do
        let env = mkEnv c "alice"
        _ <- handleValue env (rpc 1 "initialize" (object []))
        handleValue env (toolCall 2 "whoami" [])
      case outs of
        [r] ->
          asText (r .? "result" .? "structuredContent" .? "address")
            `shouldBe` Just "s-alice"
        _ -> expectationFailure "expected one response"

  describe "protocol plumbing" $ do
    it "answers ping with an empty result" $ do
      (outs, _) <- withTestDB initialTestState $ \c ->
        handleValue (mkEnv c "alice") (rpc 7 "ping" (object []))
      outs `shouldBe` [object ["jsonrpc" .= ("2.0" :: Text), "id" .= (7 :: Int), "result" .= object []]]

    it "ignores notifications" $ do
      (outs, _) <- withTestDB initialTestState $ \c ->
        handleValue
          (mkEnv c "alice")
          (object ["jsonrpc" .= ("2.0" :: Text), "method" .= ("notifications/initialized" :: Text)])
      outs `shouldBe` []

    it "rejects unknown methods with -32601" $ do
      (outs, _) <- withTestDB initialTestState $ \c ->
        handleValue (mkEnv c "alice") (rpc 1 "resources/list" (object []))
      case outs of
        [r] -> r .? "error" .? "code" `shouldBe` Number (-32601)
        _ -> expectationFailure "expected one response"

    it "rejects malformed frames" $ do
      (outs, _) <- withTestDB initialTestState $ \c ->
        handleLine (mkEnv c "alice") "this is not json\n"
      case outs of
        [r] -> r .? "error" .? "code" `shouldBe` Number (-32700)
        _ -> expectationFailure "expected one response"

  describe "tools/list" $ do
    it "lists all 12 tools with schemas" $ do
      (outs, _) <- withTestDB initialTestState $ \c ->
        handleValue (mkEnv c "alice") (rpc 2 "tools/list" (object []))
      case outs of
        [r] -> case r .? "result" .? "tools" of
          Array ts -> do
            length ts `shouldBe` 12
            [asText (t .? "name") | t <- foldr (:) [] ts]
              `shouldBe` map
                Just
                [ "whoami"
                , "claim_name"
                , "release_name"
                , "retire_name"
                , "publish_profile"
                , "discover"
                , "request"
                , "call"
                , "reply"
                , "notify"
                , "messages"
                , "purge"
                ]
          _ -> expectationFailure "expected a tools array"
        _ -> expectationFailure "expected one response"

    it "rejects unknown tools with -32602" $ do
      (outs, _) <- withTestDB initialTestState $ \c ->
        handleValue (mkEnv c "alice") (toolCall 3 "frobnicate" [])
      case outs of
        [r] -> r .? "error" .? "code" `shouldBe` Number (-32602)
        _ -> expectationFailure "expected one response"

  describe "end-to-end delegation over the protocol" $ do
    it "claim, request by name, piggyback delivery with reply duty, reply, closure" $ do
      (checks, _) <- withTestDB initialTestState $ \c -> do
        let alice = mkEnv c "alice"
            bob = mkEnv c "bob"
        setRandomInts [0xabcd, 0x1234, 0x5678]
        -- bob claims the role.
        [claimR] <- handleValue bob (toolCall 1 "claim_name" [("name", "nixos")])
        -- alice delegates by name.
        [reqR] <-
          handleValue
            alice
            (toolCall 2 "request" [("to", "nixos"), ("description", "deploy it")])
        -- bob's next tool interaction piggybacks the pending request.
        [inboxR] <- handleValue bob (toolCall 3 "whoami" [])
        let reqId = reqR .? "result" .? "structuredContent" .? "message" .? "message_id"
        -- bob replies terminally.
        [replyR] <- case asText reqId of
          Just rid ->
            handleValue
              bob
              (toolCall 4 "reply" [("in_reply_to", String rid), ("event", "completed"), ("summary", "done")])
          Nothing -> pure [Null]
        -- alice checks closure.
        [threadR] <- case asText reqId of
          Just rid ->
            handleValue alice (toolCall 5 "messages" [("scope", "thread"), ("thread", String rid)])
          Nothing -> pure [Null]
        pure (claimR, reqR, inboxR, replyR, threadR)
      let (claimR, reqR, inboxR, replyR, threadR) = checks
      asText (claimR .? "result" .? "structuredContent" .? "name") `shouldBe` Just "nixos"
      asText (reqR .? "result" .? "structuredContent" .? "message" .? "message_id")
        `shouldBe` Just "20260101-000000-alice-abcd"
      case inboxR .? "result" .? "structuredContent" .? "new_messages" of
        Array ms -> do
          length ms `shouldBe` 1
          let delivered = head (foldr (:) [] ms)
          asText (delivered .? "message" .? "message_id")
            `shouldBe` Just "20260101-000000-alice-abcd"
          (delivered .? "reply_duty") `shouldSatisfy` \case
            String t -> "terminal" `T.isInfixOf` t
            _ -> False
        other -> expectationFailure ("expected new_messages, got " <> show other)
      (replyR .? "result" .? "isError") `shouldBe` Null
      asText (threadR .? "result" .? "structuredContent" .? "thread_status" .? "state")
        `shouldBe` Just "terminal"

    it "nudges a nameless session in a git workspace, and stops once the role is taken" $ do
      ((nudged, quiet), _) <- withTestDB initialTestState $ \c -> do
        addDir "/ws/alice/.git"
        let env = mkEnv c "alice"
        [r1] <- handleValue env (toolCall 1 "whoami" [])
        [_] <- handleValue env (toolCall 2 "claim_name" [("name", "alice")])
        [r3] <- handleValue env (toolCall 3 "whoami" [])
        pure (r1, r3)
      case nudged .? "result" .? "structuredContent" .? "warnings" of
        Array ws -> do
          length ws `shouldBe` 1
          let w = head (foldr (:) [] ws)
          asText (w .? "code") `shouldBe` Just "session-unnamed"
          (w .? "message") `shouldSatisfy` \case
            String t -> "claim_name" `T.isInfixOf` t && "'alice'" `T.isInfixOf` t
            _ -> False
        other -> expectationFailure ("expected a session-unnamed warning, got " <> show other)
      (quiet .? "result" .? "structuredContent" .? "warnings") `shouldBe` Null

    it "renders domain failures as isError tool results with the taxonomy code" $ do
      (outs, _) <- withTestDB initialTestState $ \c ->
        handleValue
          (mkEnv c "alice")
          (toolCall 1 "request" [("to", "ghost"), ("description", "x")])
      case outs of
        [r] -> do
          r .? "result" .? "isError" `shouldBe` Bool True
          asText (r .? "result" .? "structuredContent" .? "code")
            `shouldBe` Just "unknown-recipient"
        _ -> expectationFailure "expected one response"
