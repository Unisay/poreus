# ADR-0014: Layered delivery, session liveness, and the OQ-1 disposition

## Status

Accepted — 2026-08-14. Resolves spec OQ-1 (delivery channel into an
idle session) at the requirements level; the channel-push layer stays
gated on a live host prototype.

## Context

RECV-1 requires automatic delivery: attendance begins with the
session, ≤ 5 s latency while the session is active, exactly once per
acknowledged stream, with the reply duty carried in-band. MCP gives a
server three ways to reach its session, none of which alone covers
every state the session can be in:

- Tool results reach the model only when the model calls tools.
- Hooks fire only at host-defined moments (session start, prompt
  submit, post-tool).
- Server→client notifications terminate at the *host application*;
  nothing in the MCP protocol obliges the host to inject them into
  the model's context. Claude Code's **channels** research preview
  (`--channels` + `--dangerously-load-development-channels
  server:poreus`) is the documented affordance that does inject
  `notifications/claude/channel` frames — gated on launch flags and
  (unverified) org policy.

Separately, peers need presence: posts to unbound names fail fast
(ADR-0012), so "is this role served right now?" must be answerable.

## Decision

**Delivery is layered; only acknowledged paths advance the cursor.**

1. **Piggyback** — every successful tool result appends
   `new_messages` (mailbox past the cursor); the read and the cursor
   write share one `BEGIN IMMEDIATE` transaction. Delivered requests
   carry a `reply_duty` line.
2. **Hook companion** — `poreus hook` on `SessionStart` and
   `UserPromptSubmit` (plain stdout → context; other events via
   `hookSpecificOutput.additionalContext`), advancing the cursor the
   same transactional way. The transaction makes hook and server
   mutually safe — no double delivery within a mailbox.
3. **Channel push** — the server tick emits
   `notifications/claude/channel` frames for messages beyond both the
   cursor and its own pushed floor. Best-effort and unacknowledged →
   **never advances the cursor**; a rare channel-then-piggyback
   duplicate is acceptable and recognizable by `message_id`. Meta
   keys are underscore-only (hyphenated keys are silently dropped by
   the host). The server declares
   `capabilities.experimental["claude/channel"]` and pushes only
   after `initialize`.
4. **Instructions** — the reply duty and the channel format live in
   `initialize.instructions`, so the receiving model needs no
   external document.

**Liveness** = not ended AND (serving pid + boot id corroborate when
known) AND heartbeat within 15 s (the tick beats every 5 s). The hook
refreshes heartbeats but never overwrites pid/boot — it is not the
serving process. Liveness gates name resolution (ADR-0012) and is
visible in the catalog (presence, DISC-4).

**Identity chain**: `$POREUS_SESSION_ID` override →
`$CLAUDE_CODE_SESSION_ID` (observed, not documented — never a single
point of failure) → `host_sessions` map keyed by (claude-ancestor
pid, boot id) → minted id persisted to that map. If
`CLAUDE_CODE_SESSION_ID` turns out to rotate across `--resume`, a
resumed session is simply a fresh address and stranded work is
recovered via adoption — the spec's "stable across resumes" is a
best-effort property of the chain, not a guarantee.

## Consequences

- Without channels, an idle session still receives everything at its
  next interaction (hook/piggyback); channels only tighten idle
  latency. The cutover does not depend on the org-policy gate.
- Until a channel-equivalent push demonstrably covers cold idle on
  this host, consumers should not assume sub-turn wake-up for idle
  peers; the catalog's presence data says whether a request will be
  seen promptly.
- The pushed-floor bookkeeping is in-memory per server instance; a
  server restart may re-push recent messages once. Duplicates remain
  recognizable by id.
