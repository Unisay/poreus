module Poreus.HostSessionSpec (spec) where

import Data.Text (Text)
import Test.Hspec

import Poreus.HostSession
import Poreus.TestM

-- | A real session file, copied from this host on 2026-08-19. Kept
-- verbatim so a change in the host's shape shows up as a test failure
-- rather than as a silent Nothing in production.
realFile :: Text
realFile =
  "{\"pid\":13211,\"sessionId\":\"c8ae64a5-03e1-4f32-bfa0-445553e024d1\"\
  \,\"cwd\":\"/etc/nixos\",\"startedAt\":1786812216781,\"procStart\":\"6460\"\
  \,\"version\":\"2.1.232\",\"peerProtocol\":1,\"kind\":\"interactive\"\
  \,\"entrypoint\":\"cli\",\"messagingSocketPath\":\"/run/user/1000/cc-socks/13211.sock\"\
  \,\"name\":\"kairos-hermes\",\"nameSince\":1786900941848,\"status\":\"idle\"\
  \,\"updatedAt\":1787081924146,\"statusUpdatedAt\":1787081924146\
  \,\"formerNames\":[{\"name\":\"x\",\"until\":1786900941848}]\
  \,\"bridgeSessionId\":\"session_014nHZB5HHQUpazxJGk2MZqb\"}"

spec :: Spec
spec = do
  describe "hostSessionPathOf" $ do
    it "reads the TARGET process's profile, not ours" $ do
      -- One poreus store can serve several host profiles, and their
      -- session files do not share a directory. Measured 2026-08-26:
      -- looking in our own profile called three live sessions broken
      -- while their files sat one directory over.
      let st =
            execTestM
              ( setEnv "CLAUDE_CONFIG_DIR" "/work"
                  >> setProcEnv 42 "CLAUDE_CONFIG_DIR" "/personal"
              )
              emptyTestState
      evalTestM (hostSessionPathOf 42) st
        `shouldBe` "/personal/sessions/42.json"

    it "falls back to our own $CLAUDE_CONFIG_DIR when the target's is unreadable" $ do
      -- The process is gone, or the kernel exposes no procfs. This is
      -- the pre-ADR-0019 behaviour and no worse than it.
      let st = execTestM (setEnv "CLAUDE_CONFIG_DIR" "/cfg") emptyTestState
      evalTestM (hostSessionPathOf 42) st `shouldBe` "/cfg/sessions/42.json"

    it "falls back to $HOME/.claude when nothing is set anywhere" $ do
      evalTestM (hostSessionPathOf 42) emptyTestState
        `shouldBe` "/home/test/.claude/sessions/42.json"

  describe "readHostSession" $ do
    it "reads every field poreus depends on" $ do
      let st =
            execTestM
              ( setEnv "CLAUDE_CONFIG_DIR" "/cfg"
                  >> addFile "/cfg/sessions/13211.json" realFile
              )
              emptyTestState
      case evalTestM (readHostSession 13211) st of
        Nothing -> expectationFailure "expected a parsed session"
        Just hs -> do
          hsPid hs `shouldBe` Just 13211
          hsName hs `shouldBe` Just "kairos-hermes"
          hsStatus hs `shouldBe` Just "idle"
          hsStatusUpdatedAt hs `shouldBe` Just 1787081924146
          hsCwd hs `shouldBe` Just "/etc/nixos"
          hsSocketPath hs `shouldBe` Just "/run/user/1000/cc-socks/13211.sock"

    it "reads procStart even though the host stores it as a string" $ do
      -- Every other numeric field is a JSON number; this one is not.
      let st =
            execTestM
              ( setEnv "CLAUDE_CONFIG_DIR" "/cfg"
                  >> addFile "/cfg/sessions/13211.json" realFile
              )
              emptyTestState
      (evalTestM (readHostSession 13211) st >>= hsProcStart) `shouldBe` Just 6460

    it "is Nothing for a missing file, never an error" $ do
      let st = execTestM (setEnv "CLAUDE_CONFIG_DIR" "/cfg") emptyTestState
      evalTestM (readHostSession 999) st `shouldBe` Nothing

    it "is Nothing for malformed JSON — the host may change shape" $ do
      let st =
            execTestM
              ( setEnv "CLAUDE_CONFIG_DIR" "/cfg"
                  >> addFile "/cfg/sessions/1.json" "{not json"
              )
              emptyTestState
      evalTestM (readHostSession 1) st `shouldBe` Nothing

    it "tolerates a file with none of the fields we want" $ do
      let st =
            execTestM
              (setEnv "CLAUDE_CONFIG_DIR" "/cfg" >> addFile "/cfg/sessions/1.json" "{}")
              emptyTestState
      (evalTestM (readHostSession 1) st >>= hsName) `shouldBe` Nothing

    it "reads a session belonging to another host profile" $ do
      -- The regression this whole path was rewritten for: the file
      -- exists, one directory over, and our own profile has nothing at
      -- that pid.
      let st =
            execTestM
              ( do
                  setEnv "CLAUDE_CONFIG_DIR" "/work"
                  setProcEnv 13211 "CLAUDE_CONFIG_DIR" "/personal"
                  addFile "/personal/sessions/13211.json" realFile
              )
              emptyTestState
      (evalTestM (readHostSession 13211) st >>= hsName)
        `shouldBe` Just "kairos-hermes"

    it "does not read OUR profile's file for a pid that lives elsewhere" $ do
      -- Pids are per-host, not per-profile, so the same number can name
      -- a file in the wrong profile. Reading it would report another
      -- window's name as this session's.
      let st =
            execTestM
              ( do
                  setEnv "CLAUDE_CONFIG_DIR" "/work"
                  setProcEnv 13211 "CLAUDE_CONFIG_DIR" "/personal"
                  addFile "/work/sessions/13211.json" realFile
              )
              emptyTestState
      evalTestM (readHostSession 13211) st `shouldBe` Nothing
