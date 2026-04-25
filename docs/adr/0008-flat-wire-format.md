# ADR-0008: Flat JSON wire format at row level

## Status

Accepted — 2026-04-25.

## Context

Two camps for JSON wire formats exist:

1. **Flat row.** Every protocol-level field is a top-level
   key. Consumers see them as columns; payload is one of those
   keys, semi-opaque.
2. **Nested envelope.** A header object with metadata, a
   distinct body for application data; consumers parse the
   envelope first, then descend into body.

poreus has both shapes in its history: the row-level
`messages` JSON is mostly flat, but `RequestPayload`/
`ReplyPayload` types layered task-shaped fields under
`payload`. The result was a mix-of-models that confused
non-Claude-Code consumers.

## Decision

Stay flat at the row level. The canonical message shape is:

```json
{
  "message_id": "...",
  "from": "...",
  "to": "...",
  "kind": "request" | "notice",
  "in_reply_to": "..." | null,
  "subscribe": [...] | null,
  "payload": <arbitrary JSON>,
  "created_at": "..."
}
```

Every protocol field has a top-level key. `payload` is the only
opaque slot — its shape is consumer convention, not protocol.

This format is identical for `send` input (without
`message_id`/`created_at`, which the CLI generates) and `inbox`
output. No envelope layering, no nested metadata.

## Consequences

**Positive**

- One shape to teach to humans, one to test, one to grep.
- Non-Claude-Code consumers can write minimal serializers —
  no special-case `RequestPayload`/`ReplyPayload` types.
- Adding a protocol field is one schema column + one JSON key,
  not a versioned envelope.

**Negative**

- Consumers can't distinguish "protocol metadata" from
  "application payload" at a glance — they must know which
  fields belong to which layer. Mitigated by the design doc
  freezing the protocol field set.
- If poreus adds a future field that conflicts with a
  consumer's payload key, namespacing is hard. Acceptable —
  payload is its own JSON object, isolated.

## Alternatives Considered

- **Envelope wrapper** (`{header: {...}, payload: ...}`).
  Cleaner separation but more nesting; non-Claude-Code clients
  would still need to learn the header shape. Marginal benefit
  for marginal cost; rejected.
- **CBOR or msgpack on the wire.** Off-topic for this redesign;
  human-readable JSON is more important than compactness for a
  local-host system.

## References

- Plan: `/home/yura/.claude-work/plans/robust-dazzling-flute.md`
- Related: ADR-0002, ADR-0003.
