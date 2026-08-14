module Poreus.IdentitySpec (spec) where

import qualified Data.Text as T
import System.Exit (ExitCode (..))
import Test.Hspec

import Poreus.Identity
import Poreus.TestM
import Poreus.Types

spec :: Spec
spec = do
  describe "resolveIdentity (REG-2)" $ do
    it "prefers $POREUS_SESSION_ID" $ do
      (i, _) <- withTestDB initialTestState $ \c -> do
        setEnv "POREUS_SESSION_ID" "override-1"
        setEnv "CLAUDE_CODE_SESSION_ID" "claude-1"
        setEnv "CLAUDE_PROJECT_DIR" "/ws/repo"
        resolveIdentity c
      idAddress i `shouldBe` SessionAddress "s-override-1"
      idSource i `shouldBe` SourceEnvOverride

    it "uses $CLAUDE_CODE_SESSION_ID next" $ do
      (i, _) <- withTestDB initialTestState $ \c -> do
        setEnv "CLAUDE_CODE_SESSION_ID" "claude-1"
        setEnv "CLAUDE_PROJECT_DIR" "/ws/repo"
        resolveIdentity c
      idAddress i `shouldBe` SessionAddress "s-claude-1"
      idSource i `shouldBe` SourceClaudeEnv
      idWorkspace i `shouldBe` "/ws/repo"

    it "mints and persists an id keyed by the claude ancestor, and reuses it" $ do
      ((i1, i2), _) <- withTestDB initialTestState $ \c -> do
        setEnv "CLAUDE_PROJECT_DIR" "/ws/repo"
        setMyPid 300
        addProc 300 (ProcInfo (Just 200) "poreus" True)
        addProc 200 (ProcInfo (Just 100) "claude" True)
        addProc 100 (ProcInfo (Just 1) "zsh" True)
        setRandomInts (cycle [0xab, 0xcd])
        i1 <- resolveIdentity c
        -- A respawned server in the same host session: new pid, same
        -- claude ancestor.
        setMyPid 301
        addProc 301 (ProcInfo (Just 200) "poreus" True)
        i2 <- resolveIdentity c
        pure (i1, i2)
      idSource i1 `shouldBe` SourceMinted
      idSource i2 `shouldBe` SourceHostMap
      idAddress i2 `shouldBe` idAddress i1
      T.length (idSessionId i1) `shouldBe` 32

    it "falls back to the repo root of cwd for the workspace" $ do
      (i, _) <- withTestDB initialTestState $ \c -> do
        setEnv "POREUS_SESSION_ID" "x"
        addProcess "git" ["-C", "/home/test", "rev-parse", "--show-toplevel"] (ExitSuccess, "/home/repo\n", "")
        resolveIdentity c
      idWorkspace i `shouldBe` "/home/repo"

  describe "mintSessionId" $ do
    it "is 32 lowercase hex chars, fully determined by the RNG script" $ do
      let st = execTestM (setRandomInts [0 ..]) initialTestState
          (sid, _) = runTestM mintSessionId st{tsRandomInts = [0, 1, 2, 3, 4, 5, 6, 7]}
      sid `shouldBe` "00000001000200030004000500060007"

  describe "findClaudeAncestor" $ do
    it "finds the nearest ancestor whose name starts with claude" $ do
      let fixture = do
            setMyPid 300
            addProc 300 (ProcInfo (Just 200) "poreus" True)
            addProc 200 (ProcInfo (Just 100) "claude" True)
            addProc 100 (ProcInfo (Just 1) "zsh" True)
          st = execTestM fixture emptyTestState
      evalTestM findClaudeAncestor st `shouldBe` Just 200

    it "returns Nothing when no claude ancestor exists" $ do
      let fixture = do
            setMyPid 300
            addProc 300 (ProcInfo (Just 100) "poreus" True)
            addProc 100 (ProcInfo (Just 1) "zsh" True)
          st = execTestM fixture emptyTestState
      evalTestM findClaudeAncestor st `shouldBe` Nothing
