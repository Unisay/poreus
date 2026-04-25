# ADR-0002: Two message kinds — `request` and `notice`

## Status

Accepted — 2026-04-25.

## Context

Schema v2 had `messages.kind ∈ {request, reply}`. The `reply` kind
was conceptually narrow: a terminal answer to a request, carrying
a `status` (completed/failed) and result artifacts.

But many useful messages don't fit "terminal answer":

- "I started working on this" (lifecycle ping mid-flight).
- "I'm stuck, may take longer than expected" (lifecycle warning).
- Broadcasts unrelated to any prior request ("protocol upgraded,
  please reinit").
- Acknowledgements that don't carry artifacts.

Forcing all of these into `reply` either pollutes its semantics
or requires inventing new top-level kinds, growing the protocol
surface every time a new use case appears.

## Decision

Reduce to two kinds:

- `request` — sender asks for work or attention.
- `notice` — everything else: lifecycle events, broadcasts,
  acknowledgements, terminal answers.

A reply (in the SMTP sense) is just a `notice` with non-null
`in_reply_to`. The semantics of a notice come entirely from its
`payload` — poreus does not interpret it.

## Consequences

**Positive**

- The protocol vocabulary fits on one line.
- New use cases (lifecycle events, broadcasts) require zero
  schema changes.
- The `[POREUS:IN]/[POREUS:RESULT]` two-prefix output collapses
  into a single `[POREUS:IN]` line with a sub-category. Watchers
  parse one pattern.

**Negative**

- Existing code paths that special-cased `reply` (worklist
  reconstruction, status display) must read `payload.event` or
  similar consumer convention. Slightly more verbose, more
  flexible.
- `kind=notice` is a broad bucket — diagnostics like "show me
  unanswered requests" must filter by `in_reply_to IS NULL` or
  query explicitly, not rely on `kind=reply`.

## Alternatives Considered

- **Three kinds: request, reply, notice.** Modest growth in
  surface for marginal gain. `reply` is a special case of
  notice (`in_reply_to ≠ NULL`); SQL distinguishes it cheaply.
- **Tagged union in payload, single `kind=message`.** Pushes
  too much into payload and loses the cheap snapshot filter
  `WHERE kind='request'` that worklist views rely on.

## References

- Plan: `/home/yura/.claude-work/plans/robust-dazzling-flute.md`
- Related: ADR-0001, ADR-0007, ADR-0008.
