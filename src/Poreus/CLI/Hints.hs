-- |
-- = Misuse hints for the poreus CLI
--
-- Telemetry from agent sessions shows two recurring failure modes:
--
--   1. Stale v0.1 subcommands (@complete@, @claim@, @reject@,
--      @status@, @watch-check@, @migrate@) — pure transport has none
--      of these.
--   2. @inbox@ called with flags that live on a sibling subcommand
--      (@--limit@/@--json@ → @history@) or were imagined (@--to@,
--      @--verb@, @--unread@, ...).
--
-- This module is a presentation-layer helper: it inspects raw argv
-- and returns a stderr hint string. It does not talk to the database
-- or read any environment.
module Poreus.CLI.Hints
  ( legacySubcommandHint
  , inboxFlagHint
  ) where

import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T

-- | If the first argv token is a removed v0.1 subcommand, return the
-- replacement hint. The caller should print it to stderr and exit
-- with code 2 (@ExitBadArgs@) without invoking optparse.
legacySubcommandHint :: [String] -> Maybe Text
legacySubcommandHint (sub : _) = case sub of
  "complete" ->
    Just $
      hint
        "complete"
        [ "to mark a request done, send a notice instead:"
        , "  poreus send --to <peer> --kind notice --in-reply-to <msg-id> --event completed --summary \"...\""
        ]
  "claim" ->
    Just $
      hint
        "claim"
        [ "there is no 'claim' in v0.2 — recipients pick up requests by reading the inbox:"
        , "  poreus inbox --open                                        # unanswered requests"
        , "  poreus send --to <peer> --kind notice --in-reply-to <msg-id> --event started"
        ]
  "reject" ->
    Just $
      hint
        "reject"
        [ "to decline a request, send a notice with event=failed or aborted:"
        , "  poreus send --to <peer> --kind notice --in-reply-to <msg-id> --event aborted --summary \"...\""
        ]
  "status" ->
    Just $
      hint
        "status"
        [ "there is no single 'status' verb; read messages directly:"
        , "  poreus inbox --in-reply-to <msg-id>                        # replies for one request"
        , "  poreus history --thread <msg-id>                           # full thread (request + replies)"
        ]
  "watch-check" ->
    Just $
      hint
        "watch-check"
        [ "there is no health check; the follower is just 'poreus inbox -f'."
        , "Exit codes 64 (already running for this session) and 65 (held by another session) are diagnostic."
        ]
  "migrate" ->
    Just $
      hint
        "migrate"
        [ "schema migration is automatic on every DB open (ADR-0009). No manual step is needed."
        ]
  _ -> Nothing
legacySubcommandHint [] = Nothing

hint :: Text -> [Text] -> Text
hint verb body =
  T.intercalate
    "\n"
    ( ("error: subcommand '" <> verb <> "' was removed in v0.2 (pure transport).")
        : zipWith (<>) prefixes body
    )
  where
    -- "hint:  " on the first line, matching indentation on follow-ups.
    prefixes = "hint:  " : repeat "       "

-- | If argv targets the @inbox@ subcommand and uses a flag that
-- belongs to a sibling or is invented, return a one-line hint to
-- append after optparse's @Invalid option@ message.
--
-- Returns 'Nothing' when nothing suspicious is detected; the caller
-- prints optparse's stock message in that case.
inboxFlagHint :: [String] -> Maybe Text
inboxFlagHint args
  | not (firstNonFlagIs "inbox" args) = Nothing
  | hasFlag "--limit" rest =
      Just "hint: --limit lives on 'history', not 'inbox'. Try: poreus history --limit N"
  | hasFlag "--json" rest =
      Just "hint: 'inbox' emits JSON by default. For tabular/JSON history, use: poreus history [--json]"
  | hasFlag "--to" rest =
      Just "hint: 'inbox' reads messages addressed to you. To filter by sender, use --from <alias>."
  | any (`hasFlag` rest) ["--verb", "--unread", "--message-id", "--all", "--id", "--status"] =
      Just "hint: 'inbox' filters are --kind, --in-reply-to, --from, --since, --open, --alias. See 'poreus inbox --help'."
  | otherwise = Nothing
  where
    rest = drop 1 (dropWhile (/= "inbox") args)

-- | True iff the first token that is not a global flag equals the
-- given subcommand name. @poreus@ has no global flags today, but the
-- check is robust to future ones (@-v@, @--verbose@, etc.).
firstNonFlagIs :: String -> [String] -> Bool
firstNonFlagIs sub = go
  where
    go [] = False
    go (x : xs)
      | "-" `isPrefixOf` x = go xs
      | otherwise = x == sub

-- | True iff any argv token is exactly @flag@ or starts with @flag=@.
hasFlag :: String -> [String] -> Bool
hasFlag flag = any match
  where
    match x = x == flag || (flag ++ "=") `isPrefixOf` x
