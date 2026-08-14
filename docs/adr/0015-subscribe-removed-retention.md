# ADR-0015: subscribe removed; fixed reply convention; unified retention

## Status

Accepted — 2026-08-14. Supersedes ADR-0003 (subscribe as a
first-class field).

## Context

ADR-0003 let each request declare which lifecycle events the sender
wanted (`subscribe: ["started", …]`), trading noise control for: a
wire field, a cross-field CHECK (subscribe only on requests),
per-delivery obligation plumbing, and a silent-typo failure class
(`"complete"` vs `"completed"` subscriptions matching nothing). At
this product's scale — tens of names, dozens of sessions, a handful
of notices per request — nobody needs per-request noise control.

Separately, v0.2 never deleted anything: messages, dead registrations
and cursors accumulated forever, and the v3 model adds per-session
records that would grow without bound.

## Decision

1. **`subscribe` is removed.** The reply convention is fixed (POL-1)
   and carried with every delivered request and in the server
   instructions: **always exactly one terminal notice** (`completed`,
   `failed`, or `aborted`) with a summary; `started` when the work is
   more than momentary; `stuck` when blocked. The vocabulary remains
   recommended-not-enforced (ADR-0007): custom events are legal, and
   the derived thread status simply treats them as non-terminal.
2. **One retention window** (default **30 days**,
   `$POREUS_RETENTION_DAYS`) governs everything ephemeral: messages,
   and ended/stale sessions' records (cursors cascade; name bindings
   reset). Names and profiles are never swept — only explicitly
   retired. The sweep runs at server start and hourly;
   `poreus admin purge` / the `purge` tool trim earlier.

## Consequences

- Two or three notices arrive per request whether or not the sender
  cares; at C-10 scale this is noise-free enough, and the typo
  failure class is gone.
- The window must comfortably exceed typical role-succession gaps:
  expiring an ended session's mailbox removes its unadopted open
  requests from the adoption query. 30 days is generous for a
  single-user host; operators trim earlier explicitly.
- Late replies stay inspectable (thread view) for the whole window
  even when the requester session is long gone.
