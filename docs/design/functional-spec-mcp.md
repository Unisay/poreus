# poreus — Functional Specification for the MCP Reimplementation

**Status:** **implemented in v0.3.0** (2026-08-15). Rev 3 of the spec
— the session identifier is the sole delivery key; names resolve at
send time and never reroute stored messages; registration optional;
delivery automatic; simplification pass applied (see §2) — was
realised in full; see §10 for how each open question was settled and
[`protocol.md`](protocol.md) for the resulting contract. This document
is kept as the requirements record: it is the mapping from the v0.2
product to v0.3, so it describes retired surface on purpose.
**Purpose:** the authoritative functional specification for a
from-scratch reimplementation of poreus as an MCP server. Every
scenario supported by the current product (v0.2 CLI + `/poreus:*`
consumer layer) is mapped here — kept, revised, absorbed, or
explicitly dropped — and new scenarios enabled by the pivot are
marked as such.
**Non-purpose:** implementation. This document names no storage
technology, no transport framing, no programming language, no
process topology beyond what the requirements themselves force.
**Sources:** `docs/design/protocol.md` (v1), ADR-0001..0009, the
`poreus` skill (including its misuse-telemetry cheat-sheet), and the
seven `/poreus:*` slash commands.

Disposition tags used throughout:

- **KEPT** — same functional behavior as v0.2.
- **REVISED** — same underlying need, changed contract.
- **NEW** — did not exist in v0.2.
- **ABSORBED** — was consumer-layer (skill/slash command) behavior,
  now a product responsibility.
- **DROPPED** — intentionally not carried over (see §11).

---

## 1. Product summary

poreus ferries structured messages between AI agent sessions on one
host. Every session is addressable from its first contact — an
address is provisioned automatically, with no registration step.
Sessions may optionally claim a stable human-friendly name and
publish a capability profile (summary, tags, typed RPC endpoints)
into the shared registry; other sessions discover them, send
free-text or typed requests, receive lifecycle notices, and
reconstruct closure from reply threads.

The current product exposes this as a CLI (`poreus <subcommand>`)
plus a skill and slash commands that teach agents how to hold the
CLI correctly. The reimplementation exposes it as an **MCP server**:
the agent's host connects the server per session, operations are
typed tool calls with schemas, and the guidance that today lives in
a skill document travels inside the tool contracts themselves.

## 2. What the pivot changes functionally

The MCP pivot is not a transport swap; it dissolves an entire layer
of the current product:

1. **Structured input replaces text plumbing.** The v0.2 `send`
   surface has three input modes (stdin JSON, flag-mode,
   `--summary-file`/`--payload-file`) that exist solely because
   shell quoting mangles rich text (apostrophes, Cyrillic, EM-DASH).
   Typed tool arguments eliminate the whole class. None of these
   modes carries forward as a distinct feature.
2. **Schemas replace documentation-as-defense.** The skill's
   cheat-sheet ("do NOT `poreus complete` / use `send --kind notice
   --event completed`") documents misuse patterns observed across
   1409 sessions. Those misuses are shape errors: one generic `send`
   verb forced agents to reconstruct conventions from memory. The
   new surface is purpose-built per intent (request / call / reply /
   notify), so the wrong shape is unexpressible rather than
   documented-against.
3. **Structured output replaces line parsing.** The
   `[POREUS:IN] ...` single-line format existed for a Monitor tool
   to grep. Delivery now returns structured messages; no consumer
   parses prefixes.
4. **Session identity becomes structural.** The fcntl lock, pidfile,
   and `$CLAUDE_CODE_SSE_PORT` token existed because short-lived CLI
   processes had no session identity. An MCP server instance is
   session-scoped by construction; the *invariants* (a single
   consumer per mailbox, takeover negotiation) survive, the
   mechanism does not appear in the contract.
5. **The consumer layer shrinks.** Of the seven slash commands,
   most reduce to "call the corresponding operation": their residual
   value (interactive profiling, autonomy policy) is specified here
   as documented contracts (§6.6) so the product carries them.
6. **Registration stops being a prerequisite.** In v0.2 an agent had
   to exist in the registry (`init`/`register`) before it could
   participate, and identity was per-repo. Sessions now receive
   their own address automatically on first contact; claiming a name
   and publishing a profile enrich discovery and enable stable
   repo-role addressing, but are never required for sending,
   receiving, or replying.
7. **The watcher disappears as a concept.** v0.2 required a
   long-running `inbox -f` under a Monitor tool, guarded by an fcntl
   lock, with exit codes 64/65 and a `/poreus:watch` command to
   start it — and delivery silently stopped whenever nobody
   remembered to start it. Delivery is now an ambient property of a
   connected session: attendance begins with the session itself
   (RECV-1), and the only conflict left is who holds a shared name
   (RECV-2).

What the pivot does **not** change: poreus remains a transport
(ADR-0001). It stores payloads verbatim, does not model tasks, and
does not enforce lifecycle vocabulary (ADR-0007). One v0.2 decision
is deliberately reversed by the simplification pass below
(ADR-0003's `subscribe`), and one carefully-bounded exception is
added (THRD-2, derived thread status).

**Simplification pass (rev 3).** Applied wherever it substantially
reduces protocol or contract surface:

- **A — `subscribe` is removed** (reverses ADR-0003). The reply
  convention is fixed instead (POL-1): always a terminal notice;
  `started` for non-momentary work; `stuck` when blocked. At this
  product's scale (C-10), per-request negotiation of two extra
  notices bought a wire field, a cross-field validation rule,
  per-delivery obligation plumbing, and a silent-typo failure class
  — for noise control nobody needs.
- **B — one arguments object for typed calls** (SEND-2). The
  positional-argument-plus-named-params split was an artifact of the
  `poreus://` URL syntax.
- **C — endpoints slim down** (REG-4): `arg_schema`/`param_schema`
  are replaced by one optional free-text usage hint. v0.2 never
  validated the schemas anyway (its `match-endpoint --arg` flag was
  dead), and LLM callers read descriptions, not schemas.
- **D — one query surface**: RECV-3, RECV-4, RECV-6, and THRD-1 are
  scenarios over a single message-query operation with composable
  filters (to/from/involving, kind, since, open, thread, limit) —
  four read paths in v0.2, one here.
- **E — one retention window** (MAINT-1): message retention and
  ended-session garbage collection collapse into a single age-based
  policy — one knob instead of two mechanisms.
- **F — adoption is a query, not a mechanism** (RECV-4): the server
  contributes only the `to_name` annotation and filters; deciding
  *when* adopting a stranded request is appropriate is consumer
  policy (POL-4).

## 3. Goals and non-goals

**Goals**

- G1. Deliver structured messages between agent sessions on the same
  host, durably, with addressing and correlation.
- G2. Maintain a capability registry: which agents exist, what they
  can do, how autonomously each capability may be exercised.
- G3. Make correct use the path of least resistance for an LLM
  agent: typed operations, self-describing contracts, guardrails at
  the point of error rather than in external documentation.
- G4. Keep the protocol small enough to fit on one page.
- G5. Stay consumer-policy-agnostic in storage: poreus does not know
  what a "task" is or what "done" means.
- G6. Zero-setup participation: a fresh session can send, receive,
  and reply with no registration, initialization, or configuration
  step. Registration exists, adds value, and is optional.

**Non-goals**

- N1. Multi-host delivery. Single-host remains the model.
- N2. Authentication or spoofing protection. All sessions belong to
  one user on one machine; trust equals filesystem access.
- N3. Guaranteed delivery beyond durable local storage: no
  acknowledgement windows, no retries, no dead-letter queue.
- N4. Task lifecycle as authoritative state. Any status view is a
  derived, convention-based projection, never a state machine
  (ADR-0001).
- N5. Human-facing UI. The consumer is an agent; humans see whatever
  the agent renders.

## 4. Actors

| Actor | Description |
|---|---|
| **Requester agent** | An agent session delegating work: discovers capabilities, sends requests, awaits notices, reconstructs closure. |
| **Responder agent** | An agent session receiving work: attends its inbox, applies autonomy policy, executes, emits lifecycle notices. |
| **Self-profiling agent** | An agent session describing its own repo's capabilities into the registry (usually with user confirmation). |
| **User** | The human owning all sessions. Confirms profiles, arbitrates autonomy prompts, resolves attendance conflicts. |
| **Operator** | The same human wearing an admin hat: deploys, backs up, purges, performs clean-slate cutovers. |

One session typically plays requester and responder simultaneously.

## 5. Domain model

- **session** — one agent session (one host-side agent conversation
  / process context). The unit that sends, receives, and attends.
  Every session has exactly one auto-provisioned session address.
- **address** — what a sender may write in `to`. Two forms:
  a **session address** — auto-generated at the session's first
  contact (derived from, or equal to, the host's session
  identifier), stable across resumes of the same session, requiring
  no user action; and a **name** — a short, unique, human-friendly
  identifier (e.g. `nixos`, `folios`) claimed voluntarily by a
  session (REG-3), with a workspace-derived default suggestion
  (repo-root basename, or a per-repo override file). Names are a
  resolution layer only: at post time the server resolves a name to
  the session currently bound to it, and the message is stored and
  delivered keyed by that session address — the one and only
  delivery key. Later rebinding never reroutes stored messages.
- **mailbox** — the durable message queue of one session, keyed by
  its session address. Exactly one mailbox per session; names have
  no mailbox of their own. A mailbox outlives its session — late
  replies remain inspectable and open requests remain adoptable
  (RECV-4) — until garbage collection (MAINT-1).
- **binding** — the claim linking one live session to a name. At
  most one session is bound to a name at a time. A binding affects
  only how *future* posts resolve; claiming, releasing, or taking
  over a name never moves or reroutes existing messages — renaming
  cannot interfere with in-flight delivery.
- **profile** — summary, tags, and endpoints attached to a *name*,
  surviving sessions. Session addresses appear in the catalog with
  workspace facts but carry no curated profile.
- **endpoint** — a typed capability a named agent offers: `verb`
  (kebab-case, unique per name), `description`, `autonomy`
  (`auto` | `confirm`), and an optional free-text usage hint
  (simplification C).
- **message** — the atomic delivery unit. Flat record (ADR-0008):
  unique time-ordered id; `from` and `to` as session addresses (the
  delivery keys); the as-written designators preserved as
  annotations (`to_name`, and `from_name` when the sender was bound)
  for display, audit, and adoption queries; `kind`; optional
  `in_reply_to`; opaque `payload`; server-assigned creation
  timestamp. Immutable once posted.
- **kind** — `request` or `notice`, the protocol's only
  classification (ADR-0002). A reply is a notice with non-null
  `in_reply_to`.
- **lifecycle vocabulary** — recommended, not enforced (ADR-0007):
  `started`, `stuck`, `completed`, `failed`, `aborted`. `completed`,
  `failed`, `aborted` are the terminal events. The emission
  convention is fixed (POL-1); v0.2's per-request `subscribe`
  declaration is removed (simplification A). Consumers may extend
  the vocabulary; unknown events are legal.
- **thread** — a request plus every notice whose `in_reply_to`
  points at it, in chronological order. Derived, not stored.
- **attendance** — the state of a session continuously receiving new
  messages from its own mailbox. Attendance is automatic — it begins
  with the session and requires no command (RECV-1). Single-consumer
  is structural: a mailbox has exactly one owning session.
- **cursor** — per-session high-water mark of messages already
  delivered through attendance. Advances only via attendance
  delivery, never via snapshot queries (ADR-0005). Lives and dies
  with its session.

## 6. Functional scenarios

Each scenario lists: actor, trigger, behavior, outcome, origin in
v0.2, and disposition.

### 6.1 Identity and registration (REG)

**REG-1 — Implicit bootstrap.** *(REVISED — replaces `init`)*
Actor: any. Trigger: first operation of any kind on a host where no
poreus store exists. Behavior: the store and schema come into
existence transparently; no operation ever fails with "not
initialized". Outcome: every other scenario works on a fresh
machine. Origin: `poreus init` (explicit) + `DB.migrate` called by
every handler (implicit). Change: the explicit `init` operation is
removed; bootstrap is a property, not a verb.

**REG-2 — Automatic session address provisioning.** *(REVISED + NEW)*
Actor: any session. Trigger: the session's first contact with the
server; also an explicit "who am I" query at any time. Behavior: the
session is assigned its session address automatically — no
registration, no confirmation — and a catalog entry is created
carrying the address, the workspace path, and the start time. The
address is stable across resumes of the same session. The identity
query returns: session address, bound name (if any), workspace.
Outcome: a fresh session can immediately send, receive, and be
replied to; `from` and "me" are never ambiguous and never supplied
by the caller. Origin: per-invocation cwd derivation
(`Repo.cwdAlias`, `.poreus/alias`). Change: identity is per-session
rather than per-repo and exists with zero setup; the
workspace-derived name moves to REG-3 as a default suggestion.

**REG-3 — Claim a name (optional registration).** *(REVISED)*
Actor: any session. Trigger: the session wants a stable,
human-friendly identity — typically the repo role (`nixos`,
`folios`). Behavior: claim a unique name; the default suggestion is
workspace-derived (repo-root basename, or the per-repo override
file), but any free name may be claimed. Claim resolution: free
name, or name whose previously bound session is gone → claimed; name
bound to a live session → refused with identification of the holder,
claimable only via explicit takeover (RECV-2). Re-claiming from the
same session is idempotent. A claim changes only how *future* posts
to the name resolve (SEND-5): no messages, mailboxes, or cursors
move — which is exactly why renaming cannot interfere with in-flight
delivery. Releasing a name — explicitly or by session end — leaves
the name and its profile intact for the next claimant; open requests
resolved to a previous holder stay in that holder's mailbox and are
recoverable by adoption (RECV-4). Outcome: posts addressed to the
name reach whichever session holds it at post time; the identity
outlives any one session. Everything except name-addressed routing
and profiles works without this step. Origin: `register ALIAS PATH`
(mandatory in v0.2) + the follow-lock's one-holder-per-alias
invariant. Change: registration is optional; the single-holder
invariant moves from the attendance lock to the name binding; names
are pure send-time resolution, never mailboxes.

**REG-4 — Publish capability profile.** *(REVISED)*
Actor: self-profiling agent. Trigger: agent announces or refreshes
what its repo can do. Behavior: atomically replace the summary,
tags, and full endpoint set attached to a *name* (publishing implies
claiming the name if not yet held, REG-3). Endpoint records are
validated structurally (verb format and uniqueness, autonomy enum;
per simplification C an endpoint carries at most a free-text usage
hint, no schemas). The profile persists with the name across
sessions. Outcome: catalog reflects the new profile; the
count of endpoints and the update timestamp are returned. Origin:
`put-profile` (which required prior `register`, else exit 3).
Change: profiles attach to durable names, not repos; the "agent not
registered" failure mode disappears; input is structured, not stdin
JSON.

**REG-5 — Interactive self-profiling.** *(ABSORBED, consumer flow)*
Actor: self-profiling agent + user. Trigger: user asks the session
to announce its capabilities. Behavior (documented contract, not a
server operation): the agent inspects its own repo directly (it has
file access; no helper needed), drafts summary / 3–8 tags / lean
endpoint set, applies the autonomy rule of thumb (read-shaped verbs
→ `auto`, mutating verbs → `confirm`, unsure → `confirm`), shows the
draft to the user for confirmation, then claims the name and
publishes (REG-3 + REG-4). If the name already carries a profile,
the agent surfaces it and asks before overwriting.
Outcome: grounded, user-approved profile. Origin:
`/poreus:register` + `inspect-repo`. Change: `inspect-repo` is
dropped (§11) — the agent's own file tools replace it.

**REG-6 — Retire a name.** *(NEW)*
Actor: operator or the bound session. Trigger: a repo is deleted or
renamed, or a role should stop advertising capabilities. Behavior:
delete the name — its profile, endpoints, and catalog entry; message
history involving the name is not rewritten. Distinct from merely
releasing a binding (REG-3), which keeps the name and profile for
the next claimant. Session addresses need no retirement — they
expire via retention (MAINT-1). Open: whether pending (unanswered)
requests originally addressed to the retiring name are surfaced at
retirement time (OQ-5). Origin: none — v0.2 had no removal path
short of hand-edited SQL.

### 6.2 Discovery and routing (DISC)

**DISC-1 — Browse the catalog.** *(REVISED)*
Actor: requester. Trigger: "who is out there / who can do X-ish
things". Behavior: list both kinds of catalog entry — named agents
(name, summary, tags, endpoints, current binding) and sessions
(session address, workspace, bound name if any, liveness) —
filterable by tag, by offered verb, restricted to one address, or
narrowed to live sessions. Auto-provisioned sessions appear without
any registration. Outcome: structured catalog of everyone
addressable right now. Origin: `discover [--tag] [--verb]
[--agent]`. Change: unnamed sessions are first-class entries;
liveness and bindings are visible.

**DISC-2 — Find providers of a verb.** *(REVISED — folded into
discovery)*
Actor: requester (usually mid-auto-routing). Trigger: the requester
has formed a `(verb, args)` hypothesis. Behavior: exact-match
lookup of a verb across all named agents, returning one candidate
per `(name, endpoint)` pair with the endpoint's autonomy, description,
and usage hint, optionally narrowed by tag. No fuzzy matching.
Outcome: 0, 1, or N routing candidates. Origin: `match-endpoint
--verb [--tag]` (the `--arg` flag was accepted and ignored — that
dead parameter is dropped). Change: this is a query mode of the
catalog, not a separate top-level concept.

**DISC-3 — Auto-routing decision.** *(ABSORBED, consumer policy)*
Actor: requester. Trigger: user asks for work without naming a
target, or the session discovers work that belongs elsewhere.
Behavior (documented contract): form a verb hypothesis; run DISC-2;
exactly one candidate → send a typed call (SEND-2) without asking;
zero or multiple → ask the user, offering the candidates and a
free-text fallback (SEND-1). Outcome: deterministic routing
procedure shared by all consumers. Origin: `/poreus:send`
auto-routing section of the skill.

**DISC-4 — Presence visibility.** *(NEW)*
Actor: requester. Trigger: deciding whether to delegate now.
Behavior: every catalog entry indicates liveness — for a session,
whether it is alive (and hence attending, RECV-1) and since when;
for a name, whether a live session is currently bound. Because posts
to an unbound name fail fast (SEND-5), presence is the pre-flight
check before delegating to a role. Outcome: the requester knows
whether a request will be seen promptly, will wait for an idle
session's next interaction, or would fail outright. Origin: none —
v0.2 kept attendance state in a lock file invisible to peers. (No
longer an open question: per-session addressing already requires the
catalog to track sessions, so presence falls out of the model.)

### 6.3 Outbound messaging (SEND)

**SEND-1 — Free-text request.** *(KEPT, structured)*
Actor: requester. Trigger: delegating work described in prose.
Behavior: post a `request` to a target address with a description,
optionally an expected-outcome statement, and optional additional
payload fields. The server stamps sender identity (REG-2), unique
time-ordered id, and creation timestamp. Outcome: the created
message (including its id, which the requester keeps for closure) is
returned. Origin: `send` with `payload.request_kind=freetext` plus a
`subscribe` list — the list is gone (simplification A): the reply
convention is fixed by POL-1 rather than negotiated per request.

**SEND-2 — Typed endpoint call.** *(REVISED)*
Actor: requester. Trigger: invoking a specific capability on a
specific agent. Behavior: post a `request` addressed by structured
coordinates — target address (usually a name, since endpoints attach
to names), verb, and a single named-arguments object (simplification
B: the positional-argument-plus-query-params duality was an artifact
of the URL syntax). The endpoint need not exist in the catalog at
send time (the target may have registered it since the caller last
looked), but the operation warns when it does not. Outcome: created
message returned. Origin: `/poreus:call` +
`payload.request_kind=rpc` + the `poreus://alias/verb[/arg][?k=v]`
URL scheme. Change: structured coordinates are canonical; the URL
becomes at most a display/shorthand notation (OQ-6); percent
encoding ceases to be something agents construct by hand.

**SEND-3 — Lifecycle reply.** *(REVISED — purpose-built)*
Actor: responder. Trigger: progress or completion of work on a
received request. Behavior: post a `notice` that *requires* a
correlation id (`in_reply_to`) and an event name, with an optional
summary and optional artifact list (type/value/description records
by convention). The recommended vocabulary is surfaced in the
contract as suggested values; arbitrary event names remain legal
(ADR-0007). Guardrails at post time: (a) warn when the referenced
thread already contains a terminal reply from this sender
(supersedes the v0.2 practice of checking `history --thread` first);
(b) the v0.2 freeform-reply warning is structurally obsolete —
this operation cannot be invoked without an event. Outcome: created
notice returned. Origin: `send --kind notice --in-reply-to X --event
E --summary …`, the freeform-reply stderr warning, and the
cheat-sheet's `complete`/`reject` replacement recipes.

**SEND-4 — Uncorrelated notice.** *(KEPT)*
Actor: any agent. Trigger: broadcast-style information not tied to a
prior request ("protocol upgraded, please re-register") or an
unsolicited ping. Behavior: post a `notice` to one address with no
correlation id; a summary or event is recommended but not required.
Multicast to several addresses is N posts (deferred, OQ-7). Outcome:
created notice returned. Origin: `send --kind notice` without
`--in-reply-to`.

**SEND-5 — Posting guarantees and validation.** *(REVISED)*
Invariants on every post: (1) sender identity, message id, and
timestamp are server-assigned — `from` is always the sender's
session address, annotated with `from_name` while the sender holds a
name (OQ-10), and the caller can forge none of them; (2) messages
are immutable once posted; (3) the sender's address is
auto-provisioned on first contact (REG-2) — no registration precedes
the first send; (4) name resolution happens at post time: a `to`
name is resolved to the session currently bound to it and the
message is stored against that session address (the name preserved
as `to_name`); later rebinding never reroutes it. Resolution
failures are immediate and explicit — a never-claimed name →
`unknown-recipient`; a claimed name with no live bound session →
`name-unbound` (the queue-for-absent-role alternative is
deliberately rejected, OQ-12); a session address whose session has
ended → accept with a warning (the send/session-end race makes
rejection wrong, and the mailbox persists until retention removes
it). v0.2 silently accepted and black-holed everything. The
`subscribe`-on-notice rule vanished with the `subscribe` field
itself (simplification A). Origin: `validateAndSend` + the
`subscribe`-on-notice CHECK.

### 6.4 Inbound delivery and consumption (RECV)

**RECV-1 — Automatic delivery (attendance).** *(REVISED)*
Actor: responder. Trigger: none — attendance begins implicitly at
the session's first contact with the server (REG-2) and lasts for
the session's lifetime. There is no watch/follow command to run,
and nothing to forget to run. Behavior: every message in the
session's mailbox — names having already been resolved to session
addresses at post time (SEND-5) — is delivered to the session
exactly once per attendance stream, in `created_at` order. Latency: within a
small bound (target: ≤ 5 s of posting) while the session is actively
working; a session idle between turns receives pending messages no
later than its next interaction with the server, and true idle
wake-up is mechanism-dependent (OQ-1). Delivered requests carry
their full structured form *plus* a reminder of the reply duty
(POL-1), so the receiving model needs no external document to act
correctly. The per-session cursor advances only through this
delivery (ADR-0005). Single-consumer holds structurally: a mailbox
has exactly one owning session. Outcome: the responder reacts to
work without polling, without parsing terminal lines, and without
starting anything. Origin: `inbox -f` + fcntl lock + 5-second tick +
`[POREUS:IN]` line format + `/poreus:watch` Monitor wrapper. Change:
attendance is no longer opt-in — the watch command family disappears
entirely; line format and lock mechanics leave the contract; the
reply duty rides along with delivery; the *channel into an idle
session* remains the pivot's
central open design question (OQ-1) — this scenario fixes the
requirements any mechanism must satisfy, not the mechanism.

**RECV-2 — Name takeover.** *(REVISED)*
Actor: responder + user. Trigger: a session wants a name currently
bound to another live session (e.g. two sessions open in the same
repo, both wanting the repo role). Behavior: the claim (REG-3) is
refused with an identification of the holder; the caller may
explicitly take over, which detaches the previous binding cleanly —
the displaced session keeps its address, its mailbox, and every
in-flight thread; takeover moves no messages, it only changes where
future name-addressed posts resolve. Bindings held by dead sessions
are detected and claimable without takeover. Attendance itself can
no longer conflict: each session attends exactly its own mailbox. Outcome: the single-consumer-per-name invariant holds without
manual lock surgery. Origin: exit codes 64/65, pidfile session
tokens, `/poreus:watch --force`. Change: the conflict moves from
"who may run the follower for this alias" to "who holds this name";
the same-session re-attend case (v0.2 exit 64) dissolves into an
idempotent no-op.

**RECV-3 — Inbox snapshot.** *(KEPT)*
Actor: any agent. Trigger: on-demand look at messages addressed to
me. Behavior: side-effect-free query, never touching the cursor
(ADR-0005), with filters: kind, sender, correlation id, and
created-after timestamp; filters compose (RECV-3/4/6 and THRD-1 are all served by
one query operation — simplification D). Callers wanting "new since
my last look" pass their own timestamp — the caller owns it.
Outcome: matching messages, chronological. Origin: `inbox` +
`--kind/--from/--in-reply-to/--since`, minus the `--alias` override
(identity is session-bound; reading another address's traffic moves
to RECV-6 history-style queries).

**RECV-4 — Open-requests sweep and adoption.** *(REVISED)*
Actor: responder. Trigger: "what still needs my attention?" —
session start, after claiming a name, or a periodic sweep. Behavior:
return requests that have *no* notice in reply from anyone
(regardless of event vocabulary; "from anyone" rather than v0.2's
"from me" — simpler, and an already-adopted request drops out of
everyone's sweep), in two scopes: (a) default — requests addressed
to my session; (b) adoption scope, while bound to a name — requests
whose `to_name` is my name but whose target session no longer
represents it (ended, or no longer bound). Adopting one is simply
replying to it: correlation is by message id, so any session can
reply, and the requester matches on `in_reply_to`, not on the
responder's address. Whether adopting is appropriate — versus
leaving it to a still-live former holder — is consumer policy
(POL-4), not server semantics (simplification F). Composes with the
other query filters (simplification D). Outcome: actionable request
list; the recovery path for role work stranded by session death.
Origin: `inbox --open` (implies kind = request).

**RECV-5 — Catch-up after suspension.** *(KEPT, simplified)*
Actor: responder. Trigger: a session resumes after being suspended
(same session identifier, same address). Behavior: delivery resumes
from the session's persisted cursor — messages that arrived in the
gap are delivered immediately; nothing is re-delivered. A mailbox
has no predecessor by construction, so the v0.2
dead-predecessor-cursor problem cannot occur; work stranded in a
*dead* session's mailbox is recovered by adoption (RECV-4), not by
cursor transfer. First-ever delivery yields the mailbox's full
backlog (usually empty — the address is born with the session).
Origin: `watch_cursors` semantics + skill guidance. Change: exactly
one cursor per session, dissolving the v0.2 per-alias-vs-per-session
dilemma (formerly OQ-8).

**RECV-6 — Activity history.** *(KEPT, de-formatted)*
Actor: any agent; also the user debugging. Trigger: "what happened
recently?" Behavior: the most recent N messages involving an address
(sent and received, merged, newest-aware ordering), default limit
10; queryable for any address, not only one's own. Outcome: structured
rows (direction, peer, kind, event/summary digest, correlation,
timestamp). Presentation (tables) is the consumer's job. Origin:
`history [--alias] [--limit] [--json]` minus the markdown-table
mode.

### 6.5 Threads and closure (THRD)

**THRD-1 — Thread view.** *(KEPT)*
Actor: any agent. Trigger: inspecting one delegation end-to-end.
Behavior: given a message id, return the root message and every
notice correlated to it, both directions, chronological. Works from
either side of the conversation. Outcome: complete conversation
record. Origin: `history --thread <id>`.

**THRD-2 — Closure check.** *(REVISED — carefully)*
Actor: requester (usually after a terminal notice arrives, or when
impatient). Trigger: "is my request finished, and how?" Behavior:
the thread view (THRD-1) additionally reports a *derived,
convention-based* status: `open` (no reply notices), `active`
(non-terminal events seen), or `terminal` (a reply whose event is in
the recommended terminal set), together with the notice that made it
terminal. This projection is explicitly labeled as vocabulary
convention, is recomputed on read, is never stored, and is never an
input to any other behavior — preserving N4/ADR-0001 while removing
the most common consumer chore (re-implementing terminal detection).
Consumers with custom vocabularies ignore the derived field and read
the raw thread. Outcome: one call answers the closure question.
Origin: `inbox --in-reply-to REQ` + consumer-side interpretation.

### 6.6 Receiver-side policies (POL) — documented contracts

These are consumer policies (the server neither stores nor enforces
them), but they are part of the product: the spec fixes their
content, and RECV-1 requires the server to carry the reply duty with
each delivered request.

**POL-1 — Lifecycle emission duty.** *(REVISED — fixed convention)*
On executing a request: **always** emit exactly one terminal notice
(`completed` / `failed` / `aborted`) with a summary; emit `started`
when the work is more than momentary; emit `stuck` when blocked.
Every reply carries the request's id as correlation. Nothing is
negotiated per request — `subscribe` is removed (simplification A):
two or three notices per request cannot drown anyone at this scale,
and a fixed convention eliminates the silent-typo failure class that
ADR-0003 + ADR-0007 traded into the protocol.

**POL-2 — Endpoint autonomy.** *(KEPT)*
For typed calls, the endpoint's declared autonomy decides:
`auto` → execute unattended (the profile owner pre-approved);
`confirm` → ask the user first. Declared at registration (REG-5
rule of thumb), read at execution time.

**POL-3 — Graduated autonomy for free-text.** *(KEPT)*
Size up requests (Small < 5 min, Medium 5–30 min, Large > 30 min;
default Medium). Idle session: Small/Medium auto-execute, Large asks
first. Busy session: finish current work, then Small auto-executes,
Medium/Large ask (execute here / new session / abort with an
`aborted` notice / leave pending).

**POL-4 — New-session handoff and adoption policy.** *(REVISED)*
When the user chooses "new session", the busy session releases the
role name (if it holds it) and leaves the request open in its own
mailbox; the user opens a session in the target workspace, which
claims the name and picks the request up through the adoption scope
of RECV-4 — no message moves, and no abort/resend round-trip is
needed. Adoption etiquette: adopt freely when the former holder is
dead or has stepped aside (released the name); when it is alive and
lost the name via takeover mid-work, the user arbitrates —
duplication is a human call on a single-user host.

### 6.7 Maintenance and operations (MAINT)

**MAINT-1 — Unified retention.** *(NEW, open — OQ-2)*
Actor: the system (automatic), plus an operator-facing explicit
purge. Trigger: unbounded growth of the message store and of records
for long-dead sessions (v0.2 never deletes anything). Behavior: one
age-based retention window governs everything ephemeral — messages
and ended sessions' records (catalog entries, mailboxes, cursors)
expire together (simplification E). Expiring an ended session's
mailbox removes its unadopted open requests from the adoption query
(RECV-4), so the window must comfortably exceed typical
role-succession gaps. Names and profiles are never retained away —
only explicitly retired (REG-6). The explicit purge lets the
operator trim earlier. Origin: none.

**MAINT-2 — Store inspection and backup.** *(KEPT)*
Actor: operator. Trigger: debugging, backup before risky cutovers.
Behavior: the durable store lives at a stable, documented host-local
location and is inspectable/copyable with standard tooling while the
system runs. Origin: `$POREUS_HOME/db.sqlite` + README guidance +
the "snapshot before clean-slate cutovers" practice.

**MAINT-3 — Clean-slate versioning posture.** *(KEPT)*
Actor: operator. Trigger: this reimplementation, and any future
non-additive change. Behavior: no data migration from v0.2 — the
store is recreated empty; each peer re-registers on its next visit
(ADR-0006/0009 re-adopted). In-flight messages at cutover are lost
by design; the operator snapshots the old store first (MAINT-2).
Additive changes require no versioning machinery.

## 7. Complete mapping — v0.2 surface → this spec

### 7.1 CLI subcommands and flags

| v0.2 surface | Maps to | Disposition |
|---|---|---|
| `init` | REG-1 (implicit bootstrap) + REG-2 | REVISED (verb removed) |
| `register ALIAS PATH` | REG-3 | REVISED (optional name claim) |
| `put-profile ALIAS` (stdin JSON) | REG-4 | REVISED (merged with registration; structured input) |
| `inspect-repo [--path]` | REG-5 rationale | DROPPED (§11.1) |
| `discover [--tag] [--verb] [--agent]` | DISC-1 | KEPT |
| `match-endpoint --verb [--tag]` | DISC-2 | REVISED (query mode of discovery) |
| `match-endpoint --arg` | — | DROPPED (was accepted and ignored) |
| `send` stdin-JSON mode | SEND-1/2/3/4 | REVISED (split by intent) |
| `send` flag-mode (`--to --kind --in-reply-to --subscribe --event --summary`) | SEND-1/3/4 | REVISED (typed args) |
| `send --summary-file` / `--payload-file` | — | DROPPED (§11.2 — quoting workaround) |
| `send` freeform-reply stderr warning | SEND-3 guardrails | REVISED (structurally impossible) |
| `send` auto-registration side effect | SEND-5(3) / REG-2 | REVISED (session address auto-provisioned) |
| `inbox` (snapshot) | RECV-3 | KEPT |
| `inbox --kind/--from/--in-reply-to/--since` | RECV-3 filters | KEPT |
| `inbox --open` | RECV-4 | KEPT |
| `inbox --alias` (identity override) | RECV-6 (other-address queries) | REVISED |
| `inbox --in-reply-to` for closure | THRD-2 | REVISED (derived status added) |
| `inbox -f` / `--follow` | RECV-1 | REVISED (automatic; no opt-in command) |
| `inbox -f --takeover` | RECV-2 / REG-3 | REVISED (name takeover) |
| `history [--alias] [--limit]` | RECV-6 | KEPT |
| `history --json` vs markdown table | RECV-6 | REVISED (structured only; §11.3) |
| `history --thread MSG-ID` | THRD-1 | KEPT |

### 7.2 Behaviors and invariants

| v0.2 behavior | Maps to | Disposition |
|---|---|---|
| Alias from repo-root basename; `.poreus/alias` override | REG-3 | REVISED (default name suggestion; identity itself is per-session, REG-2) |
| `from` never caller-supplied | SEND-5(1) | KEPT (always the session address; `from_name` annotation) |
| Message id unique, time-ordered, sender-tagged | SEND-5(1) (format is implementation) | KEPT |
| ms-precision timestamps for correct string ordering | subsumed by "strict chronological order per recipient" (RECV-1/3) | REVISED (requirement stated functionally) |
| `subscribe` field + only-on-request CHECK (ADR-0003) | POL-1 fixed convention | DROPPED (simplification A) |
| Payload stored verbatim, never interpreted | §5 message, G5 | KEPT (THRD-2 reads, never stores) |
| Cursor advances only in follow mode (ADR-0005) | RECV-1 / RECV-3 / RECV-5 | KEPT |
| One follower per alias; 64 = mine, 65 = foreign | RECV-1 / RECV-2 | REVISED (structural per mailbox; conflict survives only as name takeover) |
| fcntl lock + pidfile + `$CLAUDE_CODE_SSE_PORT` token | RECV-1/2 requirements | DROPPED as surface (§11.4) |
| 5-second poll tick | RECV-1 latency bound | REVISED (bound, not mechanism) |
| `[POREUS:IN]` line format | RECV-1 structured delivery | DROPPED (§11.5) |
| Stdout always valid JSON (tool surface) | all outputs structured | KEPT (native to MCP) |
| JSON errors on stderr + exit codes 1/2/3/5 | §9 error taxonomy | REVISED |
| Legacy v0.1 subcommand hints; inbox flag hints | G3 (schemas prevent the misuse) | DROPPED (§11.6) |
| Concurrent multi-writer safety (busy-timeout) | C-3 | KEPT (stated functionally) |
| Recommended lifecycle vocabulary, unenforced | §5, SEND-3, THRD-2 | KEPT |
| Messages to unregistered aliases silently stored | SEND-5(4) | REVISED (reject unclaimed and unbound names; warn on ended sessions) |

### 7.3 Consumer layer (skill + slash commands)

| v0.2 consumer surface | Maps to | Disposition |
|---|---|---|
| Skill cheat-sheet (misuse table) | G3 + purpose-built SEND ops | DROPPED as artifact (§11.6) |
| `/poreus:register` interactive flow | REG-5 | ABSORBED (documented contract) |
| `/poreus:discover` | DISC-1 | ABSORBED (thin) |
| `/poreus:send` incl. auto-routing | SEND-1 + DISC-3 | ABSORBED |
| `/poreus:call` incl. URL parse + existence check | SEND-2 | ABSORBED |
| `poreus://` URL scheme | SEND-2 coordinates | REVISED (OQ-6) |
| `/poreus:inbox` sweep + autonomy handling | RECV-4 + POL-2/3 | ABSORBED |
| `/poreus:watch` Monitor wrapper + exit-code table | RECV-1/2 | DROPPED as a command (delivery is automatic) |
| `/poreus:history` | RECV-6 / THRD-1 | ABSORBED (thin) |
| Request-size + graduated-autonomy policy | POL-3 | KEPT |
| Endpoint autonomy rule of thumb | REG-5 / POL-2 | KEPT |
| Lifecycle emission duty | POL-1 + RECV-1 delivery contract | REVISED (fixed convention, simplification A; carried in-band) |
| "Verify thread terminal before closing" advice | SEND-3 guardrail (a) | ABSORBED (server warns) |

## 8. Constraints and non-functional requirements

- **C-1 Single host, single user.** All trust derives from local
  filesystem access. No network listener reachable from other hosts.
- **C-2 Session-scoped service, shared durable state.** The system
  must function with an arbitrary number of concurrent agent
  sessions (each with its own server instance or connection) over
  one shared store. No session is special; there is no daemon whose
  absence breaks snapshots, sends, or registration.
- **C-3 Concurrency safety.** Concurrent posts, snapshots, name
  claims, and many simultaneous attendance streams (one per mailbox)
  must not corrupt state or lose messages. Writers block briefly
  rather than fail fast.
- **C-4 Durability.** Posted messages, names, profiles, and cursors
  survive session exits, server restarts, and host reboots.
  Mailboxes persist past session end until retention removes them
  (MAINT-1). Bindings and liveness state need not survive a host
  reboot.
- **C-5 Ordering.** Per mailbox, delivery and query order is
  strictly chronological by creation timestamp, with a stable
  total-order tiebreak. Cursor semantics must be exact — no message
  skipped, none duplicated within one attendance stream.
- **C-6 Latency.** Attendance delivery within ≤ 5 s of post while
  the receiving session is actively working; pending messages reach
  an idle session no later than its next interaction with the
  server (idle wake-up per OQ-1). Non-attendance operations complete
  promptly (they are interactive tool calls in an agent loop).
- **C-7 Self-describing surface.** Every operation carries a schema
  and a description sufficient for an LLM agent to use it correctly
  without an external skill document. Error messages state what to
  do instead, not merely what failed.
- **C-8 Transport purity.** The store never interprets payloads;
  derived views (THRD-2) are computed on read, clearly labeled, and
  never feed back into behavior.
- **C-9 Determinism and testability.** All externally-visible
  behavior involving time, randomness, environment, filesystem, and
  processes must be exercisable in a deterministic test harness
  (exact timestamps and ids assertable). This is a requirement on
  the design, not a description of a mechanism.
- **C-10 Scale envelope.** Tens of names, dozens of concurrent
  sessions, thousands of messages, message payloads up to ~1 MB. No pagination-of-pagination or
  streaming-query machinery is warranted; retention (MAINT-1) keeps
  the store within this envelope.
- **C-11 Language.** All surfaces, contracts, and errors in English.
  Payload content is user text and may be any language.
- **C-12 Compatibility posture.** Clean slate now (MAINT-3);
  afterwards, additive evolution preferred; non-additive changes
  require an ADR and an updated single-page protocol reference.

## 9. Error taxonomy (functional)

Errors are structured results with a stable machine-readable code, a
human/agent-readable message, and — where applicable — the corrective
action (C-7). Exit codes cease to be an API.

| Code (indicative) | Condition | Replaces |
|---|---|---|
| `invalid-input` | Structurally invalid arguments (schema-level rejection preferred) | exit 2 `ExitBadArgs`, JSON parse errors |
| `unknown-agent` | Operation targets an address absent from the catalog (query context) | exit 3 `ExitNotFound` |
| `unknown-recipient` | Post addressed to a never-claimed name (SEND-5(4)) | silent black-holing |
| `name-unbound` | Post to a claimed name with no live bound session (SEND-5(4); OQ-12) | silent black-holing |
| `unknown-message` | Correlation/thread id does not exist | silent empty results |
| `name-held` | Name claim refused: bound to another live session; takeover available | exits 64/65 (64 dissolves into an idempotent success, RECV-2) |
| `storage-failure` | Durable store unavailable or corrupt | exit 5 `ExitDB` |
| `internal` | Anything else | exit 1 |

Warnings (non-blocking, attached to successful results): endpoint
not found in catalog at call time (SEND-2); thread already terminal
(SEND-3); others as guardrails accrue.

## 10. Open questions

Decisions to make before or during design; each has a leaning.

**Disposition as built (v0.3.0).** OQ-3, OQ-4, OQ-8, OQ-10, OQ-11 were
already resolved in the text below and shipped as described. Also
settled during implementation:

- **OQ-1** — the *requirements* are met by three layers (ADR-0014):
  tool-result piggyback and hook digests (both acknowledged, both
  cursor-advancing) plus best-effort channel push that never advances
  the cursor. True idle wake-up still depends on the Claude Code
  channels research preview, which remains **unverified on this
  account** — the one item genuinely still open.
- **OQ-2** — one window, **30 days**, `POREUS_RETENTION_DAYS` override,
  swept at server start and hourly (ADR-0015).
- **OQ-5** — implemented as the leaning: `retire_name` reports the
  count of open requests and proceeds; it never blocks.
- **OQ-6** — resolved beyond the leaning: the `poreus://` URL is gone
  entirely. Structured coordinates are the only form; nothing renders
  or parses the URL.
- **OQ-7** — deferred as leaned; multicast is N posts.
- **OQ-9** — decided consciously: **out** for v0.3 (ADR-0010). Non-MCP
  consumers lost direct access; a thin client over the same store
  stays additive if scriptability is ever needed.
- **OQ-12** — out, as leaned: posts to unbound names fail fast.

- **OQ-1 Delivery channel into an idle session.** RECV-1 fixes the
  requirements (automatic start, ≤ 5 s while active, once per
  stream, reply duty attached); the mechanism is the pivot's
  central design question. Feasibility note: MCP *does* provide
  server→client callbacks — one-way notifications, resource
  subscriptions (subscribe → change notifications), and
  server-initiated requests (sampling, elicitation) — but they all
  terminate at the **host application**, and nothing in the protocol
  obliges the host to inject them into the model's context or wake
  an idle session. Consequently: delivery while the session is
  actively working can ride the session's own interactions with the
  server; true idle wake-up requires a host-side affordance (a host
  that surfaces server notifications into context, host hook
  integration, or an auxiliary background channel). Must be
  prototyped against the target host before design hardens; the spec
  deliberately fixes the requirement, not the mechanism.
- **OQ-2 Retention window.** One knob (MAINT-1, simplification E):
  how long ephemeral state lives — messages, and ended sessions'
  records. It must be long enough for late replies to remain
  inspectable (THRD-1), for role successors to adopt stranded
  requests (RECV-4), and for resumed sessions to revive their
  address. Leaning: a generous fixed default (weeks), plus the
  explicit operator purge for early trimming.
- **OQ-3 Unknown recipient.** Resolved by send-time resolution
  (SEND-5): never-claimed name → reject (`unknown-recipient`);
  claimed name with no live bound session → reject (`name-unbound`);
  ended session's address → accept with a warning (racy by nature).
  The only residue is OQ-12.
- **OQ-4 Presence in the catalog (DISC-4).** Resolved: included.
  Per-session addressing already requires the catalog to track
  session liveness, so presence falls out of the model.
- **OQ-5 Retirement vs pending requests (REG-6).** Leaning: surface
  the count of open requests on retirement and proceed; do not
  block.
- **OQ-6 Fate of the `poreus://` URL.** Leaning: structured
  coordinates are canonical; keep the URL only as a compact display
  form in histories, never as an input agents must construct.
- **OQ-7 Broadcast/multicast.** Leaning: defer; point-to-point plus
  N posts covers observed usage.
- **OQ-8 Cursor scope.** Resolved: exactly one cursor per session
  (RECV-5). The dead-predecessor gap is gone — stranded role work is
  recovered by adoption (RECV-4), not by cursor transfer.
- **OQ-9 Non-MCP consumers.** v0.2 promised "any process can send
  and inbox" (scripts, cron). Leaning: not a v1 requirement; if
  scriptability proves needed, a thin non-interactive client over
  the same store is additive later. Decide consciously — this
  removes a documented v0.2 capability.
- **OQ-10 Sender identity.** Resolved by send-time resolution:
  `from` is always the session address; a bound name rides along as
  the `from_name` annotation for display and adoption queries.
  Replies therefore route to the exact session that asked; if it
  dies first, the reply remains inspectable in its mailbox (THRD-1)
  until retention.
- **OQ-11 Session garbage-collection window.** Merged into OQ-2 by
  simplification E — one retention window covers both.
- **OQ-12 Deferred delivery to unbound names.** v0.2 let a request
  queue for a role nobody currently served (the repo mailbox), and
  an earlier revision of this spec kept that via durable named
  mailboxes. Send-time resolution deliberately gives it up: posts to
  an unbound name fail fast, and the sender reacts — open a session
  there, or wait for presence (DISC-4). Decision: out for v1 —
  fail-fast + presence + adoption (RECV-4) cover the observed
  workflows without reintroducing name-keyed mailboxes. Revisit only
  if genuinely asynchronous role delegation ("leave it for whoever
  opens that repo next week") proves needed.

## 11. Explicitly dropped functionality

1. **`inspect-repo`.** Existed to gather profile-drafting signals
   (basename, CLAUDE.md excerpt, skills/commands listing, ecosystem
   flags) for the registering agent. The registering agent has
   direct file access to its own repo; the helper adds a second,
   staler view of the same facts. REG-5 keeps the flow, not the
   tool.
2. **Text-plumbing input modes** (`send` flag-mode as a distinct
   mode, `--summary-file`, `--payload-file`, stdin JSON). All were
   workarounds for shell quoting; typed arguments make them
   redundant. No replacement needed.
3. **Markdown-table output** (`history` default mode). Presentation
   belongs to the consumer; all outputs are structured.
4. **Lock/pidfile/session-token surface** (fcntl file, pidfile
   format, `$CLAUDE_CODE_SSE_PORT` coupling). The invariants live in
   RECV-1/2; the mechanism is no longer observable contract.
5. **`[POREUS:IN]` line format** and its parsing conventions
   (request-kind extraction, 120-char trimming, summary/event
   fallback). Delivery is structured; digest formatting, if any, is
   consumer rendering.
6. **Anti-misuse scaffolding**: legacy v0.1 subcommand hints, inbox
   flag hints, the skill cheat-sheet. The misuse class they defended
   against is unexpressible against typed, purpose-built operations;
   C-7 carries the residual duty.
7. **Explicit `init` verb.** Bootstrap is implicit (REG-1).
8. **`match-endpoint --arg`.** Accepted and ignored in v0.2; dead.
9. **v0.1 remnants already absent from v0.2** (`claim`, `complete`,
   `reject`, `status`, `watch-check`, `migrate`) remain removed;
   their needs are fully served by SEND-3 and THRD-1/2.
10. **Watch as a user-visible concept.** `inbox -f`, `/poreus:watch`,
    the Monitor wrapper, and exit codes 64/65 all disappear: delivery
    is automatic for every connected session (RECV-1), and the only
    surviving conflict — a contested name — is handled at claim time
    (RECV-2). Nothing needs starting, and delivery cannot be silently
    off because someone forgot a command.
11. **Mandatory registration.** Participation no longer requires any
    registry write by the caller: addresses are auto-provisioned
    (REG-2); names and profiles are optional enrichment (REG-3/4).
12. **Store-and-forward to absent roles.** In v0.2 a message to an
    alias with no live session waited in the alias's mailbox for the
    next session in that repo. With names as pure send-time
    resolution, such posts fail fast instead (`name-unbound`,
    OQ-12); continuity for work that *was* delivered comes from
    adoption (RECV-4).
13. **`subscribe` and per-request lifecycle negotiation**
    (ADR-0003) — replaced by the fixed reply convention in POL-1
    (simplification A).
14. **Endpoint argument/parameter schemas** — replaced by one
    optional free-text usage hint (simplification C); v0.2 never
    validated them anyway.
