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
    it "is the basename of the repo root when no override file is present" $ do
      let st = execTestM
            ( addProcess
                "git"
                ["-C", "/home/u/work/my-app", "rev-parse", "--show-toplevel"]
                (ExitSuccess, "/home/u/work/my-app\n", "")
            )
            emptyTestState
      evalTestM (repoAlias "/home/u/work/my-app") st `shouldBe` "my-app"

    it "uses the value from .poreus/alias when that file exists" $ do
      let st = execTestM
            ( do
                addProcess
                  "git"
                  ["-C", "/home/u/work/learn-with-ai", "rev-parse", "--show-toplevel"]
                  (ExitSuccess, "/home/u/work/learn-with-ai\n", "")
                addFile "/home/u/work/learn-with-ai/.poreus/alias" "folios\n"
            )
            emptyTestState
      evalTestM (repoAlias "/home/u/work/learn-with-ai") st `shouldBe` "folios"

    it "trims surrounding whitespace from the override line" $ do
      let st = execTestM
            ( do
                addProcess
                  "git"
                  ["-C", "/x", "rev-parse", "--show-toplevel"]
                  (ExitSuccess, "/x\n", "")
                addFile "/x/.poreus/alias" "   spaced-name\t  \n"
            )
            emptyTestState
      evalTestM (repoAlias "/x") st `shouldBe` "spaced-name"

    it "skips comment and blank lines when reading the override" $ do
      let st = execTestM
            ( do
                addProcess
                  "git"
                  ["-C", "/y", "rev-parse", "--show-toplevel"]
                  (ExitSuccess, "/y\n", "")
                addFile "/y/.poreus/alias"
                  "# repo alias overrides: first non-empty, non-comment line wins\n\nfolios\nleftover\n"
            )
            emptyTestState
      evalTestM (repoAlias "/y") st `shouldBe` "folios"

    it "falls back to the basename when the override file is empty or all comments" $ do
      let st = execTestM
            ( do
                addProcess
                  "git"
                  ["-C", "/home/u/work/app", "rev-parse", "--show-toplevel"]
                  (ExitSuccess, "/home/u/work/app\n", "")
                addFile "/home/u/work/app/.poreus/alias" "# only a comment\n\n"
            )
            emptyTestState
      evalTestM (repoAlias "/home/u/work/app") st `shouldBe` "app"

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
