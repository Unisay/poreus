# ADR-0014: Layered delivery, session liveness, and the OQ-1 disposition

## Status

Accepted — 2026-08-14. Resolves spec OQ-1 (delivery channel into an
idle session) at the requirements level; the channel-push layer stays
gated on a live host prototype.

**Partially superseded — 2026-08-18.** Layer 3 (channel push) is
withdrawn: vetoed by org policy on the work profile, and never actually
running because the emitting thread dies unsupervised. Layers 1, 2 and
the liveness definition stand. Successor:
[ADR-0017](0017-native-first-delivery.md).

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
3. **Channel push** *(WITHDRAWN 2026-08-18 — see ADR-0017)* — the server tick emits
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

**Identity chain** *(precedence inverted by
[ADR-0016](0016-host-map-authoritative-identity.md) — the host map is
now consulted before the env id, and the hook resolves through the
same chain)*. As originally accepted: `$POREUS_SESSION_ID` override →
`$CLAUDE_CODE_SESSION_ID` (observed, not documented — never a single
point of failure) → `host_sessions` map keyed by (claude-ancestor
pid, boot id) → minted id persisted to that map. If
`CLAUDE_CODE_SESSION_ID` turns out to rotate across `--resume`, a
resumed session is simply a fresh address and stranded work is
recovered via adoption — the spec's "stable across resumes" is a
best-effort property of the chain, not a guarantee.

**Verified 2026-08-15 (v0.3.0).** `CLAUDE_CODE_SESSION_ID` **does
survive `--resume`**: a headless session and its resumption reported
the same address, and the store held one `sessions` row and one cursor
— the RECV-5 catch-up path, not a new identity. The
rotation contingency above therefore stays hypothetical. Re-check after
any Claude Code upgrade: the variable is still observed, not
documented.

**Channel status, measured 2026-08-15 (v0.3.0).** *Superseded
2026-08-18 — finding 1 was a false positive and the layer is now
abandoned; see "Channel push is dead" below. Retained because the
measurement error is the instructive part.* Three separate findings,
worth keeping apart:

1. **Not vetoed by policy.** `claude --channels server:poreus
   --dangerously-load-development-channels server:poreus` starts
   normally on this (IOHK-managed) account. Note the flag shape: both
   take a *tagged entry*, `--channels <servers...>`; it is not the bare
   boolean the design notes assumed.
2. **The server emits correctly.** Driving `poreus serve` directly and
   injecting a message into its store produced
   `notifications/claude/channel` within one 5 s tick, with meta
   `{message_id, message_kind}` — underscore-only, as required.
3. **Host injection is unconfirmed.** A busy headless (`-p`) session
   launched with both flags, sent a message mid-task while provably
   live, reported seeing nothing. That is evidence about headless
   mode — single-turn, with no "between turns" moment for a channel to
   land in — and not about the server. Whether an *interactive* session
   surfaces the frame is still untested.

**Channel push is dead — measured 2026-08-18 (v0.3.2.0).** The flags
were enabled fleet-wide (nixos `7bba7d8`), tested, and reverted
(`5e366de`); `/etc/nixos/home-modules/terminal.nix` now carries a
"Do NOT re-add" comment. Layer 3 fails for two *independent* reasons,
either of which alone is fatal:

1. **Org policy vetoes it, and finding 1 above measured the wrong
   thing.** The work profile sits on an IOHK-managed account whose
   `channelsEnabled` policy refuses channels outright — *"Inbound
   messages will be silently dropped"*. `--dangerously-load-development-
   channels` skips only the approved-channel *allowlist*; org policy
   sits above it. Finding 1 checked that the **process starts**, which
   is not the same claim as **the channel is enabled**. A silent-drop
   policy is invisible at startup by construction, so "starts normally"
   could never have been evidence either way. On the personal profile
   the channel did register, and an idle session still received
   nothing after 5+ minutes — consistent with finding 3.
2. **The pusher was never running.** `tick` is the only emitter, and
   `Server.hs` forks it with a bare `forkIO` and no exception handler,
   while the main loop wraps every store touch in `try`. Any exception
   from `heartbeat` / `cursorOf` / `peekPendingSince` / `sweep` escapes
   `forever`, the thread dies silently, and nothing restarts it — the
   JSON-RPC loop keeps answering, so the server looks healthy.
   Observed: four sessions with a live `poreus serve` and a heartbeat
   ~22 h stale, **three of them stopped in the same second**
   (`2026-08-17T10:57:36Z`) — one lock-contention storm across the
   shared SQLite file, `busy_timeout` being 10 s. A same-vintage
   sibling that happened to be between ticks survived.

The second finding matters far beyond channels: a dead `tick` also
stops the heartbeat, so a live session reads `live: false`, name
resolution fails (ADR-0012 fails posts to unbound names fast), and
peers silently fall back to raw addresses. That is not hypothetical —
it misrouted a real request on 2026-08-18: `discover live_only: true`
returned `names: []` while the named session was in fact serving, and
the sender addressed a *different* session that happened to share the
workspace. The stalled hourly sweep is the same defect's third
symptom, visible as a 4.1 MB WAL.

Layer 3 is therefore withdrawn, not deferred. Its requirement — waking
an idle session — moves to the host's native cross-session messaging;
see [ADR-0017](0017-native-first-delivery.md), which also
carries the `tick` supervision fix.

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

Revised 2026-08-18, after layer 3 was withdrawn:

- **Layers 1 and 2 were the whole system all along**, not a fallback
  behind a fast path. Worst-case idle latency is therefore "until the
  next prompt or tool call", with no ceiling — measured at ~2 days on
  one session (a notice sent 2026-08-16 17:45 landed 2026-08-18 08:5x).
  Any consumer that assumed the 5 s RECV-1 bound was being met by
  layer 3 was wrong for the entire life of that layer.
- **`live: false` does not mean dead.** With `tick` unsupervised, the
  heartbeat only advances when some other path happens to touch the
  store, so liveness *undercounts* idle-but-serving sessions. Until the
  supervision fix lands, treat presence as a positive signal only:
  `live: true` is trustworthy, `live: false` is not evidence of death.
  Callers must not skip a name because its binding reads not-live —
  post to the name and let it fail.
- **`live_only: true` on `discover` is a trap for exactly this reason.**
  It returned `names: []` on 2026-08-18 while a named session was
  serving. An empty filtered view reads as "no such name" and invites a
  fallback to raw addresses, which are per-process and die on restart.
  Prefer the unfiltered view plus a name post.
- **Workspace does not identify a session uniquely.** Two live sessions
  shared one repo on 2026-08-18, so "the live session in workspace X"
  is not a safe fallback for name resolution — it silently picks one.
