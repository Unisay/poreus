# poreus

A deterministic, single-binary CLI that ferries structured messages
between agents on one host. Built primarily for delegating work between
Claude Code sessions, but the wire format and storage are agnostic to
the consumer — any process can `send` and `inbox`.

State lives in a single SQLite database. There is no daemon, no
network, no lock file dance — every subcommand is a short-lived
process, except `inbox -f` which streams new messages until killed.

## What it is (and isn't)

poreus is a **transport layer**. It knows about messages, addressing,
correlation, and follow-mode delivery. It does **not** know what a
"task" is, what "done" means, or how a conversation should be
structured. Those are consumer policies — implemented in slash
commands, scripts, or whatever sits on top of the bus.

Two message kinds:

- `request` — asks the recipient to do or notice something.
- `notice` — anything else: lifecycle pings, broadcasts, terminal
  answers to a prior request.

Notices correlate to earlier messages via `in_reply_to`. Requests can
declare `subscribe: ["started", "completed", ...]` to ask the
recipient to emit lifecycle notices. Vocabulary is recommended, not
enforced (see [ADR-0007](docs/adr/0007-vocabulary-recommended-not-enforced.md)).

Full protocol reference: [`docs/design/protocol.md`](docs/design/protocol.md).

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

### From source

```bash
nix build .#poreus
./result/bin/poreus --help
```

## Quick start

```bash
# 1. Create the local database.
poreus init

# 2. Register the current directory as an agent.
cd ~/projects/myrepo
poreus register --alias alice

# 3. Send a request to another registered agent.
echo '{
  "to": "bob",
  "kind": "request",
  "subscribe": ["started", "completed", "failed"],
  "payload": {
    "request_kind": "freetext",
    "description": "fix the flake in test/Foo.hs:42"
  }
}' | poreus send

# 4. Stream incoming messages (long-running, single-instance per session).
poreus inbox -f
# [POREUS:IN] alice request (freetext): fix the flake in test/Foo.hs:42

# 5. Reply with a lifecycle notice.
echo '{
  "to": "alice",
  "kind": "notice",
  "in_reply_to": "<message-id-from-step-3>",
  "payload": { "event": "completed", "summary": "fixed and pushed" }
}' | poreus send
```

`inbox` without `-f` is a side-effect-free snapshot (filters: `--kind`,
`--from`, `--in-reply-to`, `--since`). Only `inbox -f` advances the
per-alias delivery cursor.

## CLI surface

| Subcommand        | Purpose                                                 |
|-------------------|---------------------------------------------------------|
| `init`            | Create `$POREUS_HOME` and apply schema.                 |
| `register`        | Register cwd as an alias.                               |
| `put-profile`     | Replace summary/tags/endpoints from stdin JSON.         |
| `inspect-repo`    | Emit signals about the target repo.                     |
| `discover`        | List agents and their endpoints.                        |
| `match-endpoint`  | Find agents offering a given verb.                      |
| `send`            | Post a message (stdin JSON).                            |
| `inbox`           | Snapshot of messages, with filters.                     |
| `inbox -f`        | Long-running follower, one per `(alias, session)`.      |
| `history`         | Recent messages from/to me, table or JSON.              |

`inbox -f` exit codes: `64` already running for this Claude session,
`65` held by another session (pass `--takeover` to claim).

## Storage

`$POREUS_HOME/db.sqlite` (default `$XDG_DATA_HOME/poreus`, fallback
`~/.local/share/poreus`). The schema is four tables: `agents`,
`endpoints`, `messages`, `watch_cursors`. Inspect or back up freely
with the `sqlite3` CLI — WAL mode is enabled and `busy_timeout` is set
so concurrent followers won't fight over cursor writes.

## Development

```bash
nix develop                  # GHC + cabal + HLS + sqlite + treefmt
cabal build all
cabal test
cabal run poreus -- --help
```

The Hspec suite covers the message store, lock acquisition,
inbox snapshot/follow filtering, and JSON round-trips for the wire
format.

## Documentation

- [`docs/design/protocol.md`](docs/design/protocol.md) — wire format,
  CLI surface, schema, examples. Self-contained reference.
- [`docs/adr/`](docs/adr/) — architecture decisions, one per file:
  - 0001 — pure transport layer
  - 0002 — two message kinds (request / notice)
  - 0003 — `subscribe` as first-class request attribute
  - 0004 — `inbox -f` replaces the old `watch` subcommand
  - 0005 — cursor advances only in follow mode
  - 0006 — aggressive cleanup over backwards compatibility
  - 0007 — lifecycle vocabulary is recommended, not enforced
  - 0008 — flat JSON wire format at row level
  - 0009 — clean slate, no data migration

## Status

v0.2 is the first release of the redesigned protocol. The previous
state-machine surface (`claim`, `complete`, `reject`, `status`,
`watch-check`, `migrate`) is gone — see ADR-0001 and ADR-0006.

## License

BSD-3-Clause.
