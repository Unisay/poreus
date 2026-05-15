# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`poreus` is a single-binary Haskell CLI that ferries structured messages
between agents on one host via a local SQLite database. It is a **pure
transport layer** — it knows about messages, addressing, correlation, and
follow-mode delivery, but has no concept of "task", "done", or workflow
(those are consumer policies). Two message kinds only: `request` and
`notice` (see ADR-0002). The full wire format is in
[`docs/design/protocol.md`](docs/design/protocol.md); the *why* behind
each shape is in [`docs/adr/`](docs/adr/) — read these before changing
protocol behaviour.

## Build / test / run

The dev shell is the source of truth for tool versions (GHC 9.6.6, cabal,
HLS, fourmolu, hlint, cabal-fmt, sqlite-interactive, treefmt). Enter it
via direnv or `nix develop`.

```bash
cabal build all                      # fast iteration
cabal test                           # full hspec suite
cabal run poreus -- --help           # try the CLI
cabal test --test-options="--match \"Poreus.Inbox\""   # single spec
```

CI (`.github/workflows/ci.yml`) builds **only via Nix**, never plain
cabal:

```bash
nix build .#poreus-dynamic                          # dev binary
nix build .#poreus-tests && ./result/bin/poreus-test  # what CI runs
nix build .#poreus-static                           # musl static (x86_64-linux only)
nix build .#poreus                                  # static + upx (release artifact)
```

If your change passes `cabal test` but you suspect a packaging issue,
reproduce with `nix build .#poreus-tests` before opening a PR — that's
the gate.

## Architecture

Layered top-down:

- **`app/Main.hs`** is a one-liner that calls `Poreus.main`, which
  delegates to `Poreus.CLI.run`. All real work lives in the library.
- **`Poreus.CLI`** parses argv with optparse-applicative into a `Cmd`
  ADT, then `dispatch` runs the matching `cmdX :: IO ()`. Each command
  opens its own short-lived `DB.withDB` connection — there is no shared
  state, no daemon, no server loop. The single exception is
  `cmdInboxFollow`, which holds an fcntl lock and ticks every 5 s until
  SIGTERM/SIGINT.
- **`Poreus.DB.withDB`** is the universal entry point to the DB: it
  ensures `$POREUS_HOME` exists, opens
  `$POREUS_HOME/db.sqlite` (default `$XDG_DATA_HOME/poreus`, fallback
  `~/.local/share/poreus`), sets pragmas (`foreign_keys`, WAL,
  `busy_timeout=10000`), and converts narrow exceptions to exit-5 JSON
  errors. `migrate` is always called by command handlers and is
  idempotent (every DDL uses `IF NOT EXISTS`; data migrations use `GLOB`
  predicates that only match legacy rows). There is **no
  `schema_version` table** — see ADR-0009. Don't add one unless you have
  a real migration that needs it.
- **`Poreus.Schema`** holds the four-table schema (`agents`,
  `endpoints`, `messages`, `watch_cursors`) and the timestamp-precision
  data migrations.
- **`Poreus.Effects.*`** defines capability classes (`CanTime`,
  `CanRandom`, `CanEnv`, `CanFileSystem`, `CanProcess`). All
  non-trivial side effects in the library go through one of these
  classes, with an `IO` instance for production and pure instances in
  the test suite. **When adding code that touches the clock, the
  filesystem, RNG, env vars, or external processes, route it through
  the appropriate `Can*` class** — otherwise it can't be exercised by
  `TestM`.
- **Domain modules** (`Poreus.Profile`, `Poreus.Endpoint`,
  `Poreus.Message`, `Poreus.Inbox`, `Poreus.History`, `Poreus.Inspect`,
  `Poreus.Repo`, `Poreus.Lock`) each own one CLI verb's worth of
  behaviour and have a 1:1 hspec module in `test/`.
- **`Poreus.Types`** holds the small core (`Alias`, `TaskId`, `Agent`,
  `Endpoint`, `Autonomy`) reused by every other module. `TaskId` is
  historical naming — it's a message id, not a task id.

## Testing model

Tests live in `test/Poreus/*Spec.hs`, discovered by `hspec-discover`.
The reusable harness is **`Poreus.TestM`**:

- **`TestM`** = pure `State TestState` with fake instances of every
  `Can*` class. Use for anything that doesn't touch SQLite. `TestState`
  owns a UTC clock, a scripted RNG, an in-memory file map, an env-var
  table, and a scripted process table.
- **`TestIOM`** = `StateT TestState IO` — same fakes, plus `MonadIO` so
  you can thread a real `:memory:` SQLite connection.
- **`withMemoryDB :: TestState -> (Connection -> TestIOM a) -> IO (a, TestState)`**
  is the standard wrapper for DB-backed specs — opens a fresh
  `:memory:` DB, runs `migrate`, executes the action, closes the
  connection. Each call is isolated; concurrent specs do not share
  state.

Determinism is load-bearing: tests assert exact timestamps and exact
hex suffixes in message ids. Drive the clock via `setClock` /
`advanceClock` and the RNG via `setRandomInts`. Don't introduce direct
`IO` calls in the library when adding a feature — it will be untestable
in `TestM`.

## Inbox follow semantics (the one stateful command)

`poreus inbox -f` is the only long-running subcommand and the only
place with cross-process coordination:

- **One follower per `(alias, Claude session)`**, enforced by an fcntl
  lock at `$POREUS_HOME/.inbox-<alias>.lock` plus a pidfile carrying
  `<pid>\n<CLAUDE_CODE_SSE_PORT>\n`.
- Exit codes: `64` (already running for *this* session — refuse), `65`
  (held by *another* session — caller must pass `--takeover` to claim).
- The per-alias delivery cursor (`watch_cursors`) **advances only in
  follow mode**, never on snapshots (ADR-0005). Snapshots are
  side-effect-free.
- Multiple followers across different aliases write the cursor
  concurrently; that's why `busy_timeout=10000` is set in `DB.withDB`.
- Timestamps are millisecond-precision (`...HH:MM:SS.fffZ`) because the
  follower compares with `>`, and `.` (0x2E) lexicographically precedes
  `Z` (0x5A) — mixing precisions silently breaks ordering. The
  idempotent data migration in `Poreus.Schema.dataMigrationStatements`
  rewrites any legacy second-precision rows.

If you change anything in `Poreus.Lock`, `Poreus.Inbox`, the timestamp
representation, or the cursor logic, re-read ADR-0004 / ADR-0005 first
and update both.

## Conventions

- Cabal extensions are global: `OverloadedStrings`, `RecordWildCards`,
  `NamedFieldPuns`, `LambdaCase`, `DerivingStrategies`,
  `GeneralizedNewtypeDeriving`, etc. (full list in `poreus.cabal`).
  Don't re-enable them per-module.
- GHC warnings are strict: `-Wall -Widentities
  -Wincomplete-record-updates -Wincomplete-uni-patterns
  -Wredundant-constraints`. Fix warnings rather than `-Wno-`.
- Formatting: `fourmolu` (available in `nix develop`). Run before
  committing if you've changed `.hs` files.
- Errors from CLI commands are emitted as pretty JSON on stderr via
  `Poreus.Exit.exitJsonError`; the exit code is the integer carried by
  the `ExitKind`. New error paths should reuse this rather than calling
  `exitWith` directly.
- Stdout from `send`, `init`, `register`, `discover`, etc. is **always
  valid JSON** (used as a tool surface for slash commands). Don't add
  human-readable prose to those paths — put help text in the optparse
  `progDesc` / `footerDoc` instead.

## When changing the protocol

The protocol redesign committed a **clean slate, no migrations**
posture (ADR-0006, ADR-0009). If a change is non-additive:

1. Update `docs/design/protocol.md` so it stays the single
   self-contained reference.
2. Add an ADR (`docs/adr/NNNN-short-name.md`) capturing the rationale,
   alternatives, and what the change *forbids*. Number sequentially.
3. Lifecycle event vocabulary is **recommended, not enforced**
   (ADR-0007) — keep validation off the schema and out of `send`.
