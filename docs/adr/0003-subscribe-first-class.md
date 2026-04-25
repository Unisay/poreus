# ADR-0003: `subscribe` as a first-class request attribute

## Status

Accepted — 2026-04-25.

## Context

A request sender often wants progress updates beyond just the
final answer: "tell me when you start", "tell me if you abort".
The receiver, however, has no signal about which lifecycle
events the sender actually cares about. Without that signal the
receiver either:

- Sends nothing (sender flies blind), or
- Sends every possible event (sender drowns in noise), or
- Hard-codes assumptions (couples sender and receiver).

The best resolution is for the sender to declare, with the
request itself, which lifecycle events they want notified about.

## Decision

Add a top-level optional attribute `subscribe` to `request`
messages:

```json
{
  "to": "B",
  "kind": "request",
  "payload": { ... },
  "subscribe": ["started", "completed", "failed"]
}
```

- `subscribe` is a JSON array of event-name strings.
- It is stored in a dedicated SQLite column `messages.subscribe`,
  not buried in payload. This makes it queryable and visible in
  `inbox`-output without consumer-side parsing.
- The schema enforces `(kind='request' OR subscribe IS NULL)` —
  notices have no business carrying a subscription.
- An empty array or absent field both mean "do not send any
  lifecycle notifications, finite forget".
- The receiver-consumer reads `subscribe` and emits a `notice`
  per matching lifecycle event with `in_reply_to` pointing at
  the original request.

## Consequences

**Positive**

- Senders explicitly opt in, no implicit chatter.
- Receivers have a deterministic instruction set.
- The contract is visible at the protocol level — any client
  (not just Claude Code) can implement it without parsing
  vendor-specific payload conventions.

**Negative**

- Schema gains one column and one CHECK; tests gain a
  validation case (subscribe-on-notice → `ExitBadArgs`).
- Vocabulary is not enforced (see ADR-0007), so typos in event
  names silently produce no notification.

## Alternatives Considered

- **Subscription as payload convention** (`payload.subscribe =
  [...]`). Simpler schema, but the field becomes invisible in
  the message-row view and consumers must parse JSON to filter.
- **Server-side fan-out** (poreus generates lifecycle notices
  on its own based on receiver state). Violates ADR-0001 —
  poreus would re-acquire knowledge of task lifecycle.

## References

- Plan: `/home/yura/.claude-work/plans/robust-dazzling-flute.md`
- Related: ADR-0007 (vocabulary), ADR-0008 (wire format).
