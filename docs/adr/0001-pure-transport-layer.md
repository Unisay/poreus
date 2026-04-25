# ADR-0001: poreus is a pure transport layer

## Status

Accepted — 2026-04-25.

## Context

Earlier versions of poreus combined two distinct concerns in one CLI:

1. **Message transport** — delivering notifications between Claude Code
   sessions on the same host (table `messages`, `send`, `inbox`,
   `watch`, `history`).
2. **Task lifecycle** — a state machine over delivered work units
   (table `tasks`, columns `status`/`claimed_at`/`completed_at`,
   subcommands `claim`, `complete`, `reject`, `status`, table
   `results`).

This conflation produced concrete defects:

- The slash-command `/poreus:inbox` queried `tasks.status`, but
  `/poreus:watch` streamed `messages` — two visually similar
  entry points that answered different questions, confusing both
  users and other agents.
- Watch infrastructure leaked across two bash scripts, a slash
  command, and CLI subcommands, because the protocol layer was
  unclear about what belonged where.
- Top-level `kind ∈ {freetext, rpc}` on every message was a
  task-lifecycle classification masquerading as transport
  metadata; non-task consumers (broadcasts, lifecycle pings)
  could not be expressed cleanly.

## Decision

poreus owns **delivery only**: durable storage of `message` rows,
addressing, correlation via `in_reply_to`, and follow-mode
streaming. Anything resembling state machines, work queues, or
"what do I owe?" views is the **consumer's** responsibility.

Concretely:

- Drop tables `tasks` and `results`.
- Drop subcommands `claim`, `complete`, `reject`, `status`.
- Slash-commands rebuild any task-shaped view they need by reading
  the message stream (request + correlated notices) and applying
  their own conventions.

## Consequences

**Positive**

- Smaller blast radius for protocol changes — the wire format is
  short, well-defined, and unrelated to consumer policy.
- Other consumers (non-Claude-Code clients) can use poreus as a
  generic local message bus without inheriting Claude-Code task
  semantics.
- ~300 lines of state-machine handlers leave the CLI; tests
  shrink correspondingly.

**Negative**

- Consumers must reimplement work-tracking views. For Claude
  Code the cost lands in `/poreus:inbox` and is one-time.
- Loss of the "is this task done?" point query — replaced by
  `inbox --in-reply-to <id>` and consumer-side interpretation of
  the resulting notices.

## Alternatives Considered

- **Keep tasks as derived projection over messages.** Tempting,
  but the projection has no canonical authority — different
  consumers would compute different "status" — and SQLite cannot
  enforce idempotent finality on a view. Rejected.
- **Move state machine to a separate poreus subcommand
  family.** Still mixes concerns under one binary; better to
  externalize fully.

## References

- Plan: `/home/yura/.claude-work/plans/robust-dazzling-flute.md`
- Related: ADR-0002, ADR-0006, ADR-0009.
