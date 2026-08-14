# ADR-0012: Session address as the sole delivery key; seq ordering

## Status

Accepted — 2026-08-14. Supersedes the *mechanisms* of ADR-0004
(inbox-follow) and ADR-0005 (cursor-advance-follow-only); their
invariants survive in new form.

## Context

v0.2 keyed mailboxes by repo alias. That conflated two things — a
durable role and a live consumer — and produced the hard cases:
cursor ownership across sessions sharing an alias, the dead-
predecessor cursor gap, store-and-forward to roles nobody served, and
the fcntl follow-lock arbitrating who may consume.

Separately, v0.2 ordered and cursored by millisecond-precision
timestamp strings, where a single second-precision row silently broke
`>` comparisons — a real bug class that required idempotent data
migrations to keep at bay.

## Decision

1. **The session address is the one and only delivery key.** Every
   session gets exactly one mailbox, born with it. Names are a pure
   send-time resolution layer: a `to` name resolves to the session
   currently bound, the message is stored against that session
   address, and the as-written designators ride along as
   `from_name`/`to_name` annotations. Later rebinding never reroutes
   stored messages.
2. **Fail fast instead of store-and-forward.** Posts to a
   never-claimed name are rejected (`unknown-recipient`); posts to a
   claimed name with no live bound session are rejected
   (`name-unbound`). Continuity for work that *was* delivered comes
   from the adoption query (open requests whose `to_name` is mine but
   whose target session no longer represents it) — adopting is simply
   replying, because correlation is by message id.
3. **`seq` (INTEGER AUTOINCREMENT) is the total order and cursor
   key.** `created_at` remains for display, `since` filters, and
   retention only. The timestamp-precision hazard and its data
   migrations disappear.
4. **Exactly one cursor per session**, advanced only by acknowledged
   delivery paths (tool-result piggyback, hook digest) inside
   `BEGIN IMMEDIATE`; snapshots stay side-effect-free (the ADR-0005
   invariant, carried forward).

## Consequences

- v0.2's exit-64/65 conflicts dissolve: attendance cannot conflict
  (one mailbox, one owner); only name ownership can, and that is
  handled at claim time with explicit takeover.
- A message to a role queues nowhere when the role is unserved — the
  sender learns immediately and reacts (open a session there, or
  wait for presence). This deliberately drops v0.2's
  leave-it-for-next-week workflow (spec OQ-12) until proven needed.
- The v3 schema is a clean slate (ADR-0009 posture re-adopted);
  there is still no `schema_version` table.
