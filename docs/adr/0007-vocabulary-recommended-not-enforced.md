# ADR-0007: Lifecycle vocabulary is recommended, not enforced

## Status

Accepted — 2026-04-25.

## Context

Per ADR-0003, `subscribe` is a JSON array of event-name strings.
Per ADR-0001, poreus does not interpret payload semantics.
Question: should poreus validate that `subscribe[i]` belongs to
a fixed vocabulary (e.g., `started | stuck | completed | failed
| aborted`)?

Pros of enforcement:
- Catches typos at send time (`completd` would error).
- Discoverability via `--help`.

Cons of enforcement:
- Every new lifecycle convention requires a poreus release.
- Different consumers might want their own events
  (`peer-review-requested`, `escalated`, `paused`, etc.).
- Schema migration costs grow with vocabulary changes.

## Decision

The recommended vocabulary lives in **documentation**
(SKILL.md, design doc), not in the schema. `subscribe` accepts
arbitrary strings; receiver-consumers compare against their own
event names and ignore unknown values.

Recommended events (the canonical set Claude Code consumers
should support):

- `started` — receiver has begun working on the request.
- `stuck` — receiver is blocked or progress slow.
- `completed` — work finished successfully.
- `failed` — work attempted but failed.
- `aborted` — receiver stopped voluntarily before completion.

Consumers may extend this list. Non-Claude-Code consumers may
ignore it entirely.

## Consequences

**Positive**

- Protocol is fully forward-compatible: new events ship without
  poreus changes.
- Different communities (Claude Code agents, external clients)
  share the transport but customize the vocabulary.
- Schema is one column simpler — no enum table.

**Negative**

- Typos pass schema validation. `subscribe: ["completd"]` is a
  silent no-op. Receivers won't emit a `completd` notice; the
  sender will wait forever for a notification that never comes.
- No discoverability via CLI. Vocabulary lives in docs that
  must be kept in sync.
- Different consumers may diverge on event meaning
  (`completed` vs `done` vs `success`).

## Alternatives Considered

- **Strict enforcement with a `lifecycle_events` table and
  foreign key.** Adds a migration whenever a new event ships;
  prevents "did I mean started or starting?" ambiguity at write
  time. Rejected — too much process for a documentation
  problem.
- **Property-test linter** that warns on unknown events but
  does not error. Doable as future work; rejected for v1
  scope.

## References

- Plan: `/home/yura/.claude-work/plans/robust-dazzling-flute.md`
- Related: ADR-0003.
