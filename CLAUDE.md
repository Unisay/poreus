# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`poreus` is a single-binary Haskell **MCP server** that ferries
structured messages between AI agent sessions on one host via a local
SQLite database. It is a **pure transport layer** — it knows about
messages, addressing, correlation, and delivery, but has no concept of
"task", "done", or workflow (those are consumer policies). Two message
kinds only: `request` and `notice` (ADR-0002). The full contract is in
[`docs/design/protocol.md`](docs/design/protocol.md) (v2); the
functional requirements are in
[`docs/design/functional-spec-mcp.md`](docs/design/functional-spec-mcp.md);
the *why* behind each shape is in [`docs/adr/`](docs/adr/) — read
these before changing protocol behaviour.

One binary, four entry modes (ADR-0013/0017): `poreus serve` (the MCP
server, spawned per session by Claude Code over stdio), `poreus hook`
(short-lived hook companion — it also claims the role at SessionStart
and runs the retention sweep), `poreus
doctor` (operator cross-check), `poreus admin purge` / `poreus
version`.

## Build / test / run

The dev shell is the source of truth for tool versions (GHC 9.6.6,
cabal, HLS, fourmolu, hlint, cabal-fmt, sqlite-interactive, treefmt).
Enter it via direnv or `nix develop`.

```bash
cabal build all                      # fast iteration
cabal test                           # full hspec suite (deterministic)
cabal test --test-options="--match \"Poreus.Query\""   # single spec
./scripts/mcp-smoke.sh "$(cabal list-bin exe:poreus)"  # protocol smoke
treefmt --fail-on-change --no-cache  # format gate (fourmolu/cabal-fmt/nixpkgs-fmt)
hlint src test                       # lint gate
```

CI (`.github/workflows/ci.yml`) builds **only via Nix**, never plain
cabal:

```bash
nix build .#poreus-tests && ./result/bin/poreus-test  # the gate
nix build .#checks.x86_64-linux.poreus-smoke          # sandboxed protocol smoke
nix build .#poreus-dynamic                            # dev binary
nix build .#poreus-static                             # musl static (x86_64-linux only)
nix build .#poreus                                    # static + upx (release artifact)
nix build .#ci-lint                                   # CI's treefmt/hlint bundle
```

If your change passes `cabal test` but you suspect a packaging issue,
reproduce with `nix build .#poreus-tests` before opening a PR — that's
the gate. The sandboxed smoke check also catches "works on my machine"
assumptions (no git on PATH, empty environment).

**GHC pin quartet**: the GHC 9.6.6 pin lives in four places that must
move together — `nix/project.nix` (`compiler-nix-name`),
`nix/shell.nix` (`haskell-nix.compiler.ghc966`), the `base ^>=4.18`
bound in `poreus.cabal`, and the `ghc-version` in the darwin job of
`.github/workflows/release.yml`.

**Version** is single-sourced from `poreus.cabal`: `nix/version.nix`
parses it; Haskell code reads it via `Paths_poreus`.

## Architecture

Layered top-down:

- **`app/Main.hs`** is a one-liner calling `Poreus.main`, a hand-rolled
  `getArgs` dispatch (no optparse-applicative) to `Server.runServer`,
  `Hook.runHook`, or `Admin.runPurge`.
- **`Poreus.Server`** owns the JSON-RPC loop **and nothing else**.
  There is no background thread and none may be added without an ADR
  (ADR-0017 §2, and the Note in `Server.hs`): v0.3's tick was forked
  bare, one exception killed all three of its duties silently, and the
  server kept answering for 45 h looking healthy. One SQLite
  connection behind an MVar; stdout writes behind a second MVar.
  Graceful shutdown (stdin EOF, SIGTERM, SIGINT) marks the session
  ended and releases its role.
- **`Poreus.Mcp.*`** is the protocol layer: `Framing` (newline-delimited
  JSON-RPC over an injectable `Transport` — ADR-0011), `JsonRpc`
  (parse/build over plain aeson Values), `Protocol` (initialize with
  version negotiation and the `instructions` string carrying the reply
  duty, the addressing rule, and the doorbell rules; ping; tools/list;
  tools/call), `Tools` (the 12-tool registry with hand-built schemas —
  no TH, musl cross-build constraint), `Errors` (domain failures as
  `isError` tool results per protocol §9), `Digest` (the hook's
  model-facing rendering of a delivered message).
- **Domain modules** own one concern each, 1:1 with hspec modules:
  `Identity` (the session-id chain — env override,
  `CLAUDE_CODE_SESSION_ID`, `host_sessions` map, minting), `Session`
  (upsert/end + liveness + host-name resolution), `HostSession`
  (parsing the host's own session file), `Name` (claim/takeover/
  release/retire + role resolution + `mailboxesOf`), `Profile`,
  `Catalog` (discover), `Post` (request/call/reply/notify + guardrail
  warnings), `Query` (the one query surface: inbox/open/history/thread
  + derived thread status), `Deliver` (cursor-advancing delivery over
  a list of mailboxes), `Doorbell` (the latency hint on a post
  result), `Doctor` (operator cross-checks), `Retention`.
- **`Poreus.DB.withDB`** opens `$POREUS_HOME/db-v4.sqlite`, sets pragmas
  (**`busy_timeout` first** — concurrent opens race on
  `journal_mode=WAL` otherwise — then `foreign_keys`, WAL), runs the
  idempotent `migrate` (implicit bootstrap, REG-1), and converts
  SQLError/IOError to a `storage-failure` `PoreusException`. There is
  **no `schema_version` table** (ADR-0009/0012/0017) — the schema
  generation lives in the filename instead.
- **`Poreus.Effects.*`** defines capability classes (`CanTime`,
  `CanRandom`, `CanEnv`, `CanFileSystem`, `CanProcess`,
  `CanSystemInfo`). All non-trivial side effects go through these,
  with `IO` instances for production and pure fakes in the test suite.
  **When adding code that touches the clock, filesystem, RNG, env,
  processes, or pids/boot-id, route it through the appropriate `Can*`
  class** — otherwise it can't be exercised by `TestM`.

## Key invariants (re-read the ADR before touching)

- **The mailbox belongs to the role** (ADR-0017, reversing ADR-0012):
  a post to a known role is queued whether or not a session holds it,
  and the role's cursor comes with the role, so a successor drains its
  predecessor's backlog with no special query mode. A name that was
  never claimed still fails — a typo must not create a mailbox nobody
  drains. `seq` is the total order and cursor key (`created_at` is
  display/filter/retention only).
- **Only acknowledged paths advance the cursor** (ADR-0014/0017):
  tool-result piggyback and hook digests do, inside `BEGIN IMMEDIATE`
  (so server and hook never double-deliver). Query snapshots are
  side-effect-free. The doorbell carries no payload and advances
  nothing.
- **Reply convention is fixed, vocabulary is not enforced**
  (ADR-0015/0007): one terminal notice per request; the derived thread
  status is a labeled projection, recomputed on read, never stored,
  never an input to other behavior.
- **Never store a fact the OS or the host owns** (ADR-0017 §3).
  Liveness is the triple `(pid, boot_id, proc_start)` compared against
  the OS on every read — no stored heartbeat, ever. The hook never
  overwrites pid/boot (it isn't the serving process), so a pid-less
  row reads live and `doctor` is what flags it.
- **No error or warning text names a session address as a remedy**
  (ADR-0017, L5) — name the role, or the host session name.

## Testing model

Tests live in `test/Poreus/*Spec.hs`, discovered by `hspec-discover`.
The reusable harness is **`Poreus.TestM`**:

- **`TestM`** = pure `State TestState` with fakes for every `Can*`
  class (clock, scripted RNG, in-memory files, env table, scripted
  process table, scripted process *tree* + boot id for
  `CanSystemInfo`).
- **`TestIOM`** = `StateT TestState IO` — same fakes plus `MonadIO`
  for real SQLite.
- **`withTestDB`** — bracket-safe fresh `:memory:` DB (foreign keys
  on) + migrate. **`withTestFileDB`** — one temp-file DB, two
  connections: multi-process semantics (takeover, role succession,
  interleaved cursor advance).
- Protocol-loop tests drive `Poreus.Mcp.Protocol.handleValue` as
  `[Value] -> TestIOM [Value]` — no transport, no handles.

Determinism is load-bearing: tests assert exact timestamps and exact
message ids. Drive the clock via `setClock`/`advanceClock`, the RNG
via `setRandomInts`, the process tree via `addProc`/`setMyPid`/
`setBootId`. Don't introduce direct `IO` calls in the library when
adding a feature — it will be untestable in `TestM`.

## Conventions

- Cabal extensions are global (`OverloadedStrings`, `RecordWildCards`,
  `LambdaCase`, `DerivingStrategies`, …; full list in `poreus.cabal`).
  `ConstraintKinds` is enabled per-module where `ToolM` is used.
- GHC warnings are strict (`-Wall -Widentities` + incomplete-patterns
  and redundant-constraints). Fix warnings rather than `-Wno-`.
- Formatting: `treefmt` (fourmolu + cabal-fmt + nixpkgs-fmt), config
  in `fourmolu.yaml`/`treefmt.toml`. Run before committing; CI fails
  on drift. hlint must be clean (`.hlint.yaml` holds the few
  relaxations).
- Domain functions return `Either PoreusError a`; new error paths
  reuse the §9 taxonomy (`Poreus.Types.ErrorCode`) with a corrective
  `action` where applicable (C-7). Never exit from library code.
- All MCP outputs are structured (`structuredContent` + compact text);
  no human-readable prose on the tool surface — guidance lives in tool
  descriptions and the `instructions` string.

## When changing the protocol

The v0.4 cutover re-committed the **clean slate, no migrations**
posture (ADR-0006/0009/0010/0017) and moved the store to
`db-v4.sqlite` so an old binary never meets a new schema. If a change
is non-additive:

1. Update `docs/design/protocol.md` so it stays the single
   self-contained reference.
2. Add an ADR (`docs/adr/NNNN-short-name.md`) capturing the rationale,
   alternatives, and what the change *forbids*. Number sequentially
   (next: 0020).
3. Lifecycle event vocabulary stays **recommended, not enforced**
   (ADR-0007) — keep validation off the schema and out of the post
   path.
