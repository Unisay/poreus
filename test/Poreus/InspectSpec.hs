module Poreus.InspectSpec (spec) where

import System.Exit (ExitCode (..))
import Test.Hspec

import Poreus.Inspect
import Poreus.TestM

-- Run `inspectRepo` inside the pure TestM with a fixtured fake filesystem
-- and a scripted `git rev-parse`.
fixtureRepo :: TestM ()
fixtureRepo = do
  addProcess "git" ["-C", "/repo", "rev-parse", "--show-toplevel"]
    (ExitSuccess, "/repo\n", "")
  addDir "/repo"
  addFile "/repo/poreus.cabal" "name: test\n"
  addFile "/repo/flake.nix" "{}"
  addFile "/repo/CLAUDE.md" "line 1\nline 2\n"
  addDir "/repo/skills"
  addDir "/repo/skills/poreus"
  addDir "/repo/skills/a2a"
  addDir "/repo/commands"
  addDir "/repo/commands/git"
  addFile "/repo/commands/git/commit.md" "x"
  addFile "/repo/commands/review.md" "x"

spec :: Spec
spec = do
  describe "inspectRepo" $ do
    it "reports cabal, flake, claude-md, skills and commands" $ do
      let st = execTestM fixtureRepo emptyTestState
          r = evalTestM (inspectRepo "/repo") st
      irRepoRoot r `shouldBe` "/repo"
      irBasename r `shouldBe` "repo"
      irHasCabal r `shouldBe` True
      irHasFlake r `shouldBe` True
      irHasPackageJson r `shouldBe` False
      irClaudeMdExcerpt r `shouldBe` Just "line 1\nline 2\n"
      irSkills r `shouldMatchList` ["a2a", "poreus"]
      irCommands r `shouldMatchList` ["git:commit", "review"]

    it "returns false flags for a bare directory" $ do
      let st = execTestM (addDir "/bare" >>
                 addProcess "git"
                   ["-C", "/bare", "rev-parse", "--show-toplevel"]
                   (ExitSuccess, "/bare\n", ""))
                emptyTestState
          r = evalTestM (inspectRepo "/bare") st
      irHasCabal r `shouldBe` False
      irHasFlake r `shouldBe` False
      irHasPackageJson r `shouldBe` False
      irClaudeMdExcerpt r `shouldBe` Nothing
      irSkills r `shouldBe` []
      irCommands r `shouldBe` []
