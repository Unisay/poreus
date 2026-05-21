module Poreus.CLI.HintsSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Poreus.CLI.Hints

spec :: Spec
spec = do
  describe "legacySubcommandHint" $ do
    let mustReplace verb prefix =
          it (verb <> " is intercepted before optparse") $ do
            let Just msg = legacySubcommandHint [verb, "--whatever", "X"]
            T.unpack msg `shouldStartWith`
              ("error: subcommand '" <> verb <> "' was removed in v0.2")
            T.unpack msg `shouldContain` prefix

    mustReplace "complete" "poreus send --to <peer> --kind notice"
    mustReplace "claim" "poreus inbox --open"
    mustReplace "reject" "--event aborted"
    mustReplace "status" "poreus history --thread <msg-id>"
    mustReplace "watch-check" "poreus inbox -f"
    mustReplace "migrate" "automatic on every DB open"

    it "returns Nothing for live subcommands" $ do
      legacySubcommandHint ["inbox", "--limit", "5"] `shouldBe` Nothing
      legacySubcommandHint ["history", "--limit", "5"] `shouldBe` Nothing
      legacySubcommandHint ["send", "--to", "x", "--kind", "notice"] `shouldBe` Nothing
      legacySubcommandHint ["init"] `shouldBe` Nothing

    it "returns Nothing for empty argv" $
      legacySubcommandHint [] `shouldBe` Nothing

    it "every hint single-line opens with 'hint:'" $ do
      let Just msg = legacySubcommandHint ["complete"]
          ls = T.lines msg
      head ls `shouldBe` "error: subcommand 'complete' was removed in v0.2 (pure transport)."
      ls !! 1 `shouldBe` "hint:  to mark a request done, send a notice instead:"

  describe "inboxFlagHint" $ do
    it "catches --limit on inbox" $
      inboxFlagHint ["inbox", "--limit", "5"]
        `shouldBe` Just "hint: --limit lives on 'history', not 'inbox'. Try: poreus history --limit N"

    it "catches --limit=N form on inbox" $
      inboxFlagHint ["inbox", "--limit=5"]
        `shouldBe` Just "hint: --limit lives on 'history', not 'inbox'. Try: poreus history --limit N"

    it "catches --json on inbox" $
      inboxFlagHint ["inbox", "--json"]
        `shouldBe` Just "hint: 'inbox' emits JSON by default. For tabular/JSON history, use: poreus history [--json]"

    it "catches --to on inbox" $
      inboxFlagHint ["inbox", "--to", "alice"]
        `shouldBe` Just "hint: 'inbox' reads messages addressed to you. To filter by sender, use --from <alias>."

    it "catches invented flags on inbox" $ do
      let expected =
            Just "hint: 'inbox' filters are --kind, --in-reply-to, --from, --since, --open, --alias. See 'poreus inbox --help'."
      inboxFlagHint ["inbox", "--verb", "ping"] `shouldBe` expected
      inboxFlagHint ["inbox", "--unread"] `shouldBe` expected
      inboxFlagHint ["inbox", "--message-id", "abc"] `shouldBe` expected
      inboxFlagHint ["inbox", "--all"] `shouldBe` expected
      inboxFlagHint ["inbox", "--id", "abc"] `shouldBe` expected
      inboxFlagHint ["inbox", "--status", "done"] `shouldBe` expected

    it "ignores valid inbox flags" $ do
      inboxFlagHint ["inbox", "--from", "alice"] `shouldBe` Nothing
      inboxFlagHint ["inbox", "--kind", "notice"] `shouldBe` Nothing
      inboxFlagHint ["inbox", "--in-reply-to", "X"] `shouldBe` Nothing
      inboxFlagHint ["inbox", "--open"] `shouldBe` Nothing
      inboxFlagHint ["inbox", "-f"] `shouldBe` Nothing

    it "skips global flags before the subcommand" $
      inboxFlagHint ["--verbose", "inbox", "--limit", "5"]
        `shouldBe` Just "hint: --limit lives on 'history', not 'inbox'. Try: poreus history --limit N"

    it "does not fire when subcommand is not inbox" $ do
      inboxFlagHint ["history", "--limit", "5"] `shouldBe` Nothing
      inboxFlagHint ["send", "--to", "alice", "--kind", "notice"] `shouldBe` Nothing
      inboxFlagHint [] `shouldBe` Nothing
