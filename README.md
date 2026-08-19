# poreus

An MCP server that ferries structured messages between AI agent
sessions on one host. The agent's host (Claude Code) spawns one
`poreus serve` per session over stdio; sessions become addressable
automatically, discover each other's capabilities, exchange free-text
requests and typed RPC calls, and reconstruct closure from reply
threads.

State lives in a single SQLite database. There is no daemon and no
setup: any number of concurrent sessions, each with its own server
instance, share one store.

## What it is (and isn't)

poreus is a **transport layer**. It knows about messages, addressing,
correlation, and delivery. It does **not** know what a "task" is or
what "done" means — those are consumer policies, implemented on top.

Two message kinds:

- `request` — asks the recipient to do or notice something.
- `notice` — anything else: lifecycle pings, broadcasts, terminal
  answers to a prior request (correlated via `in_reply_to`).

The reply convention is fixed: every received request gets exactly one
terminal notice (`completed`/`failed`/`aborted`) with a summary,
`started` when the work is more than momentary, `stuck` when blocked.
Vocabulary is recommended, not enforced
([ADR-0007](docs/adr/0007-vocabulary-recommended-not-enforced.md),
[ADR-0015](docs/adr/0015-subscribe-removed-retention.md)).

Full protocol reference: [`docs/design/protocol.md`](docs/design/protocol.md).
Functional spec behind the design:
[`docs/design/functional-spec-mcp.md`](docs/design/functional-spec-mcp.md).

## Install

### Nix flake

```bash
nix profile install github:Unisay/poreus
```

Or pin the input from another flake:

```nix
{
  inputs.poreus.url = "github:Unisay/poreus";

  # …
  environment.systemPackages = [
    inputs.poreus.packages.${pkgs.system}.poreus
  ];
}
```

### Register with Claude Code

```bash
# Bare `poreus` via PATH — never a /nix/store path (it would go stale).
claude mcp add --scope user poreus -- poreus serve
claude mcp list   # verify: poreus connected
```

Optional but recommended — the hook companion delivers pending
messages as context at session start and on every prompt
(`~/.claude/settings.json`):

```json
{
  "hooks": {
    "SessionStart": [{"hooks": [{"type": "command", "command": "poreus hook"}]}],
    "UserPromptSubmit": [{"hooks": [{"type": "command", "command": "poreus hook"}]}]
  }
}
```

## How a delegation flows

Everything below is MCP tool calls made by the model — nothing is
typed by hand.

1. A session is addressable from its first contact (`whoami` shows the
   auto-provisioned `s-…` address). Optionally it claims a stable role
   name (`claim_name`, default: the repo basename) and publishes a
   capability profile (`publish_profile`).
2. A requester finds a target (`discover`, filter by tag or exact
   verb) and posts `call {to: "nixos", verb: "deploy-poreus", args:
   {…}}` or `request {to: "nixos", description: "…"}`. The message
   lands in that **role's** mailbox, which outlives every process that
   holds the role — so a peer that is restarting is not an error for
   the sender. Only a name that was never claimed is refused.
3. The responder receives the request in-band: appended as
   `new_messages` to its next tool result, or injected by the hook at
   its next prompt. Both advance the cursor transactionally, so a
   message is delivered exactly once. poreus states no latency bound;
   the guarantee is "at the recipient's next prompt or tool call".
   To make that sooner, the post result may carry a `doorbell` — the
   sending model rings the recipient **once** through the host's own
   `SendMessage`, with a fixed payload-free body, and never retries.
4. The responder emits `reply {in_reply_to, event: "completed",
   summary}`; the requester checks closure with
   `messages {scope: "thread", thread: <id>}` — the derived
   `thread_status` answers "is it finished?".

## Tool surface

`whoami`, `claim_name`, `release_name`, `retire_name`,
`publish_profile`, `discover`, `request`, `call`, `reply`, `notify`,
`messages`, `purge` — schemas and full semantics in
[`docs/design/protocol.md` §8](docs/design/protocol.md).

## Storage

`$POREUS_HOME/db-v4.sqlite` (default `$XDG_DATA_HOME/poreus`, fallback
`~/.local/share/poreus`). Seven tables: `sessions`, `cursors`, `names`,
`endpoints`, `messages`, `host_sessions`, `maintenance`. The filename
carries the schema generation on purpose — a session still running an
older binary keeps its own store instead of meeting a schema it cannot
read. Inspect or back up freely with the `sqlite3` CLI; WAL mode and
`busy_timeout` are set for the many concurrent writers, and nothing
writes unless a message moves. Retention: one 30-day window
(`POREUS_RETENTION_DAYS`) sweeps messages, ended sessions, and
orphaned cursors, from the hook path at most hourly; roles and
profiles persist until explicitly retired. `poreus admin purge
[--older-than DAYS]` trims earlier, and `poreus doctor` reports the
sweep age.

## Checking the installation

`poreus doctor` cross-checks every fact two parties can answer — what
poreus computed against what the operating system and the host say —
and exits non-zero on a disagreement. It reports; it never repairs.
Checks: computed liveness against the host's published sessions, the
host-name lease against the host's current name, a host status that
stopped moving on a live process, mail queued for a role nobody holds,
sweep recency, and write-ahead log size.

## Development

```bash
nix develop                  # GHC + cabal + HLS + sqlite + treefmt
cabal build all
cabal test                   # deterministic hspec suite
./scripts/mcp-smoke.sh "$(cabal list-bin exe:poreus)"   # protocol smoke
```

CI builds via Nix only: `nix build .#poreus-tests` is the gate,
`checks.poreus-smoke` runs the protocol smoke in the sandbox, and
treefmt/hlint run from the `ci-lint` bundle (the dev shell's exact
tool versions).

## Documentation

- [`docs/design/protocol.md`](docs/design/protocol.md) — the v3
  contract: tools, message record, delivery model, errors, schema.
- [`docs/design/functional-spec-mcp.md`](docs/design/functional-spec-mcp.md)
  — every v0.2 scenario mapped into the MCP reimplementation.
- [`docs/adr/`](docs/adr/) — architecture decisions, one per file:
  - 0001 — pure transport layer
  - 0002 — two message kinds (request / notice)
  - 0003 — `subscribe` as first-class request attribute
    (superseded by 0015)
  - 0004 — `inbox -f` replaces the old `watch` subcommand
    (mechanism superseded by 0012)
  - 0005 — cursor advances only in follow mode
    (mechanism superseded by 0012; invariant survives)
  - 0006 — aggressive cleanup over backwards compatibility
  - 0007 — lifecycle vocabulary is recommended, not enforced
  - 0008 — flat JSON wire format at row level
  - 0009 — clean slate, no data migration
  - 0010 — reimplement as an MCP server; retire the CLI
  - 0011 — hand-rolled newline-delimited JSON-RPC
  - 0012 — session address as the sole delivery key; seq ordering
  - 0013 — one binary, three entry modes (four since 0017)
  - 0014 — layered delivery, liveness, and the idle wake-up channel
    (layer 3 withdrawn by 0017)
  - 0015 — subscribe removed; fixed reply convention; unified retention
  - 0016 — the host map is the authoritative session identity
  - 0017 — native-first delivery: role mailboxes, no background
    threads, no stored liveness, no latency bound

## Status

v0.4 moves the mailbox to the role and deletes the server's background
thread (ADR-0017). Clean slate again — no data migration from the v0.3
store, which stays on disk under its own filename. The v0.2 CLI
surface is gone.

## License

BSD-3-Clause.
