module Poreus.RepoSpec (spec) where

import System.Exit (ExitCode (..))
import Test.Hspec

import Poreus.Repo
import Poreus.TestM

spec :: Spec
spec = do
  describe "repoRoot" $ do
    it "uses `git rev-parse --show-toplevel` output when the command succeeds" $ do
      let st = execTestM
            ( addProcess
                "git"
                ["-C", "/some/nested/dir", "rev-parse", "--show-toplevel"]
                (ExitSuccess, "/some/repo\n", "")
            )
            emptyTestState
      evalTestM (repoRoot "/some/nested/dir") st `shouldBe` "/some/repo"

    it "trims trailing whitespace from git output" $ do
      let st = execTestM
            ( addProcess
                "git"
                ["-C", "/x", "rev-parse", "--show-toplevel"]
                (ExitSuccess, "/x/root\r\n\t ", "")
            )
            emptyTestState
      evalTestM (repoRoot "/x") st `shouldBe` "/x/root"

    it "falls back to the input dir when git exits non-zero" $ do
      let st = execTestM
            ( do
                putProcessDefault (ExitFailure 128, "", "not a repo")
            )
            emptyTestState
      evalTestM (repoRoot "/not/a/repo") st `shouldBe` "/not/a/repo"

  describe "repoAlias" $ do
    it "is the basename of the repo root" $ do
      let st = execTestM
            ( addProcess
                "git"
                ["-C", "/home/u/work/my-app", "rev-parse", "--show-toplevel"]
                (ExitSuccess, "/home/u/work/my-app\n", "")
            )
            emptyTestState
      evalTestM (repoAlias "/home/u/work/my-app") st `shouldBe` "my-app"

  describe "cwdAlias" $ do
    it "combines getCurrentDir with repoAlias" $ do
      let st =
            execTestM
              ( do
                  addProcess
                    "git"
                    ["-C", "/home/test", "rev-parse", "--show-toplevel"]
                    (ExitSuccess, "/home/test/myrepo\n", "")
              )
              emptyTestState
      evalTestM cwdAlias st `shouldBe` "myrepo"
