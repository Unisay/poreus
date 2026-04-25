# ADR-0005: Cursor advances only in follow mode

## Status

Accepted — 2026-04-25.

## Context

`poreus inbox` shares its underlying query with `poreus inbox
-f`. The `watch_cursors` table tracks "last message I delivered
via stream" per alias.

Two natural questions:

1. Should `inbox` (snapshot) be idempotent — calling it twice
   returns the same set?
2. Should `inbox` (snapshot) advance the cursor — implying it
   "consumed" the messages?

If yes to both: contradiction. Two callers running snapshot
back-to-back would each see "everything since last time", but
the second would see fewer messages. Worse: a snapshot run
during follow mode would steal messages from the long-running
watcher's cursor.

## Decision

The cursor lives only in follow mode.

- `poreus inbox` (snapshot) is **side-effect-free**. It does
  not read or write `watch_cursors`. It returns whatever the
  filters select.
- `poreus inbox -f` (follow) is the only writer: each tick it
  reads the cursor, queries new messages, emits them, then
  advances the cursor to the latest seen `created_at`.
- Snapshot-mode users wanting "new since last check" use
  `--since <ts>` explicitly. The caller owns the timestamp.

## Consequences

**Positive**

- Snapshots are safe to run anywhere, anytime, without
  disrupting active watchers.
- Cursor semantics are a property of the streaming session, not
  a hidden side effect of read commands.
- Tests for snapshot mode don't need cursor fixtures.

**Negative**

- Snapshot callers who want "what's new" must explicitly track
  their own timestamp. Not free, but the explicitness is
  desirable.
- Two follow-mode watchers for the same alias would race for
  the cursor. Prevented by the single-instance lock (ADR-0004),
  not by cursor isolation.

## Alternatives Considered

- **Per-caller cursor (cursor key = alias + caller-id).**
  Solves multi-reader cleanly but requires every caller to
  identify itself. Overkill for a single-host system; consumers
  can just remember `--since`.
- **Snapshot advances cursor.** Easiest for users who want
  "what's new" but breaks idempotency and steals from
  watchers.

## References

- Plan: `/home/yura/.claude-work/plans/robust-dazzling-flute.md`
- Related: ADR-0004.
