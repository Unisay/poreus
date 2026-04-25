# ADR-0004: `inbox -f` replaces `watch`

## Status

Accepted — 2026-04-25.

## Context

Schema v2 had two semantically related but separate subcommands:

- `inbox` — snapshot of work for me (ran against `tasks`).
- `watch-check` — single-tick of new messages (ran against
  `messages` cursor).

A bash loop wrapped `watch-check` in `while true; sleep 5; done`
to provide streaming. Idempotency, single-instance enforcement,
session-token tracking, and process management were all done in
shell scripts (`poreus-watch-loop.sh`, `watch-alive.sh`).

After ADR-0001 (drop tasks) the two commands now query the same
table. Their only difference is run-shape: snapshot vs stream.
This is the canonical use case for the Unix `--follow`/`-f`
idiom (`tail`, `journalctl`, `kubectl logs`).

## Decision

A single subcommand `poreus inbox` handles both modes:

- Without flags: snapshot. SELECT with optional filters
  (`--kind`, `--in-reply-to`, `--from`, `--since`), prints JSON,
  exits 0.
- With `-f` / `--follow`: long-running. Acquires a per-(alias,
  session) lock (see ADR-0005, ADR-Lock module), polls the same
  query against the cursor every 5 seconds, prints
  `[POREUS:IN]` lines for the Claude Code Monitor tool.

Single-instance and session-awareness move from bash to Haskell:
flock + `CLAUDE_CODE_SSE_PORT` in pidfile. Exit codes signal the
collision case (already running for this session, or held by
another session — see Exit module).

The previous `watch-check` subcommand and all bash scripts are
removed.

## Consequences

**Positive**

- One subcommand, one set of tests, one --help to maintain.
- Bash → Haskell lift removes the `jq`/`stat`/`pkill` portability
  surface.
- Single-instance enforcement is now type-safe (Haskell modeling
  of acquire results) instead of mtime-based heuristics.
- New consumers learn one CLI primitive, not two.

**Negative**

- Snapshot and follow share a code path; care needed to keep
  snapshot side-effect-free (see ADR-0005 about cursor advance).
- Slash-command `/poreus:watch` still exists as a wrapper that
  invokes `Monitor: poreus inbox -f`, so the user-facing name
  doesn't change. Potential confusion: "watch" is no longer a
  subcommand. Documented in SKILL.md.

## Alternatives Considered

- **Two distinct subcommands with shared internals.** Cleaner
  separation, but three names for the same domain (inbox,
  watch, watch-check) is more vocabulary than the system needs.
- **`/poreus:inbox --watch` slash-command unification.** Earlier
  considered (see plan history); rejected because the slash
  layer benefits from two clear intents, even when the CLI
  layer collapses.

## References

- Plan: `/home/yura/.claude-work/plans/robust-dazzling-flute.md`
- Related: ADR-0005 (cursor), ADR-0006 (cleanup).
