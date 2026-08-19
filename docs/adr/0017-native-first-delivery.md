# ADR-0017: poreus v0.4 — native-first delivery, no background threads

## Status

Accepted — 2026-08-19. Supersedes layer 3 of
[ADR-0014](0014-delivery-channels-liveness.md) (channel push) and
narrows its liveness rule. Reverses part of
[ADR-0012](0012-session-address-delivery-key.md): the delivery key
becomes the role mailbox, and posting to a known-but-unheld role
queues instead of failing. Refines
[ADR-0016](0016-host-map-authoritative-identity.md) with a second
identity hazard. Replaces an uncommitted draft of the same number
written by the claude-config session on 2026-08-18, whose measurements
are folded in below and whose poke direction was wrong.

Design brief and adversarial review ran as poreus thread
`20260819-103921-claude-config-b523`.

## Context

### What the host now owns

Claude Code ≥ 2.1.224 ships cross-session messaging: `ListAgents`,
`SendMessage`, a per-session inbox socket, and a documented contract —
*"when the receiving session is idle, Claude Code starts a new turn
with the message"*. That is RECV-1's requirement, implemented by the
host, for free.

It also writes a per-session state file at
`$CLAUDE_CONFIG_DIR/sessions/<claude-pid>.json`, mode 0644, keyed by
exactly the pid that `Poreus.Identity.hostKey` already computes. It
carries `name`, `status`, `statusUpdatedAt`, `procStart`, `cwd`, `pid`,
`sessionId`, `messagingSocketPath`.

What the host does **not** provide, and poreus therefore keeps: a
durable message to a session that is restarting or not yet running;
role names that outlive processes; typed endpoints with autonomy flags
and the capability registry; the reply duty and derived thread status;
structured payloads of arbitrary size; history and retention.

### The week-one operational record

Nine failures, each measured, each mapped to a rule.

| # | Incident | Rule |
|---|---|---|
| L1 | `tick` forked bare (`Server.hs:79`); one exception inside `forever` killed heartbeat, push and sweep together. Four ticks died within 150 ms on 2026-08-17T10:57:36Z; twelve sessions were affected by 08-19, none recovered | no background threads |
| L2 | Stored liveness lied for 45 h. Nothing compared it against the OS | never store a fact the OS or host owns |
| L3 | Channel push was vetoed by the `channelsEnabled` org policy on the work profile (silent drop), and its emitter was dead anyway. The 2026-08-15 "not vetoed" finding measured that the process *starts* with the flags, which is not the claim that the channel is *enabled* | transport belongs to the host |
| L4 | Every `--resume` orphaned the name binding until a manual re-claim | auto-rebind at `SessionStart` |
| L5 | A `name-unbound` error whose text named an address taught a peer to address sessions directly; two days later that habit misrouted a request | errors name the role, never an address |
| L6 | Two live sessions shared one workspace; "the live session in workspace X" picked the wrong one | workspace never routes |
| L7 | `CLAUDE_CODE_SESSION_ID` differs between the server env and the tool env *within one process* | only the host map is authoritative |
| L8 | Channel frames carried payload, so dedup by `message_id` had to be load-bearing | a payload-free doorbell removes the class |
| L9 | 14+ serve processes × a 5 s heartbeat write produced SQLite lock storms against a 10 s `busy_timeout` | traffic-driven writes only |

L5 is stated precisely because the first reading of it was wrong. The
2026-08-18 misroute involved no `name-unbound` error at all — a
`discover live_only: true` returned `names: []` and the peer fell back
to workspace matching. The `name-unbound` in the record is from
2026-08-16, and what it taught was not "fail fast is bad" but "the
error text pointed at an address".

### The permission-class correction

The doorbell's latency depends on a host rule neither side had read
correctly. The class comes from the **active** permission mode:
`bypassPermissions`, or `plan` where bypass is available, classify as
`bypass`; everything else, including `auto`, classifies as
`prompting`. `--allow-dangerously-skip-permissions` only makes bypass
*available*.

All 15 live sessions on this host run `--permission-mode auto
--allow-dangerously-skip-permissions`, so the fleet is uniformly
`prompting`. Senders nevertheless assert `bypass`, so every poke hit
`mode-mismatch` and was held for human approval.

An explicit `crossSessionInbound` setting short-circuits that rule
entirely — the decision function returns the configured policy before
any class comparison. With `crossSessionInbound: "accept"` written to
both profiles on 2026-08-19 12:45:11, a poke to a session started
2026-08-17 20:48:16 was delivered **automatically**, which also proves
a running session re-reads the setting live.

## Decision

### 1. Two layers, and only the first has a guarantee

1. **Ledger** — the system of record. Every post is one row in the
   recipient role's mailbox. Drained by the hook (`SessionStart`,
   `UserPromptSubmit`) and by piggyback on tool results. Only
   acknowledged reads advance the cursor, inside `BEGIN IMMEDIATE`.
   Unchanged from ADR-0014 layers 1 and 2, which were the only layers
   that ever worked.
2. **Doorbell** — latency only. After a post to a role whose holder is
   reachable, the **sending model** rings via the host's `SendMessage`
   tool with a fixed one-line body of roughly 100 bytes.

**poreus states no latency bound.** RECV-1's "≤ 5 s while active" is
withdrawn and replaced by an explicit best-effort statement. The
doorbell can be held, refused, or lost to a class mismatch, and the
sender learns this only through an out-of-band notice it cannot branch
on. The ledger's guarantee is "next prompt or tool call", and that is
the only guarantee the protocol makes.

**Measured, and better than designed:** a `SendMessage`-initiated turn
fires `UserPromptSubmit`. So the doorbell wakes the session *and* the
hook drains the mailbox in the same turn, through the acknowledged
path. Ring, turn starts, hook delivers, cursor advances — one round
trip, not two. The "check messages" text in the poke stays as
belt-and-braces for hosts without the hook installed, but the hook
drain is the delivery path.

**Hard rule for the sending model, in the protocol instructions:**
post to the ledger, ring once, never retry, never wait, never branch
on doorbell success or failure. A retried poke turns a latency
optimisation into a denial of service against the user's attention.

**The doorbell never carries payload.** Three independent reasons: the
native transport has a size ceiling; a held poke would park a second
copy of the message where neither side controls it; and an own-child
socket write arrives framed as `role: user`, indistinguishable from
something the human typed.

### 2. No background threads, at all

`tick` is deleted, not supervised. Its three duties disappear or move:

- heartbeat → deleted; liveness is computed on read
- channel push → deleted; the doorbell is sender-side and native
- retention sweep → the hook path, behind a `last_sweep` row, at most
  hourly

This is a **chosen constraint, not a deduction**. L1's evidence shows
an unsupervised thread with coupled duties, which supervision would
also have fixed. The constraint is justified by making the failure
class unrepresentable rather than merely handled. If something later
genuinely needs a thread, that gets argued on its merits.

The server becomes stateless between calls: no threads, no
`pushedRef`, no `readyRef`. SQLite writers drop from "every server,
every 5 s" to "on traffic", which alone would have prevented the
2026-08-17 storm.

### 3. Never store a fact the OS or the host owns

| Fact | Owner | poreus's move |
|---|---|---|
| process alive | OS | computed on read; store nothing |
| session attentive | host | never claim it; the session file answers |
| waking an idle session | host | `SendMessage` |
| session identity | host map | ADR-0016 unchanged |
| reply owed, thread status | poreus | keep |
| roles, endpoints, registry | poreus | keep |
| durable backlog, retention | poreus | keep |

**Liveness is the triple `(pid, boot_id, proc_start)`, compared on
read.** A pid plus a boot id cannot detect PID reuse, and its failure
direction is *false alive* — the same lie the heartbeat told. `pid_max`
is 4194304 on this host, so reuse is rare, not impossible.
`getProcessStartTime` already exists as a `CanSystemInfo` method and
`host_sessions` already keys on the triple; `sessions` gains the same
column.

A hung-but-running process still reads as alive. That is accepted:
waking is the host's job now, and poreus only promises queueing.

### 4. Mailboxes belong to roles

A **role** is the durable principal — name, mailbox, profile,
endpoints — and survives restarts. A **session** is ephemeral, used
for reply routing and for unnamed senders, and is subject to
retention.

**Known roles queue; unknown names fail fast.** A post to a role with
a `names` row is stored in that role's mailbox even when no session
holds it, and returns a warning naming the absent holder. A post to a
name that was never claimed and carries no profile fails, because a
typo must not create a mailbox nobody will ever drain. An explicit
force flag lets a sender create-and-queue deliberately, so the choice
belongs to the sender rather than to poreus.

Error text names the role and never an address. That is L5's actual
lesson.

**The role mailbox cursor survives holder changes**, so a new holder
drains the backlog including requests left by a dead former holder.
Adoption (RECV-4) becomes structural rather than a special query mode.

**`retire_name` refuses while mail is queued**, and its force variant
reports the discarded count.

### 5. The host name is a lease, not a snapshot

The hook claims the workspace's default role at `SessionStart` when it
is unclaimed or its holder's triple is dead, and announces the claim
in its context output. A live holder means no claim.

Alongside the claim, poreus records the **host session name** read from
`$CLAUDE_CONFIG_DIR/sessions/<pid>.json`, so the doorbell targets an
exact name instead of prefix-matching a workspace-derived auto-name.
Prefix matching would reintroduce L6 through the latency layer.

That name must be **re-read on every hook invocation, not only at
claim time.** A mid-session `/rename` changes it — this ADR's own
session was renamed from `poreus-…` to `redesign` while the design was
being reviewed, which would have silently orphaned its doorbell target.

The session file is undocumented host state and may move or change
shape. Reading it is nevertheless accepted where poking another pid's
socket was rejected, because reading a status file is passive and
idempotent while poking a socket is not, and because the file is
deliberately 0644 while its sibling `.key` files are 0600.

**Trust `name`, `status`, `procStart`, `cwd`, `pid`. Never trust
`version`** — it is stamped at session start and never rewritten. Three
values were observed simultaneously on one host: `2.1.232`, `2.1.234`,
and an installed `2.1.235`.

### 6. Presence annotates, never filters

`live_only` is removed. An empty filtered view reads as "no such name"
and that is what produced the 2026-08-18 misroute. Presence is an
annotation: `holder_process: alive|dead`, an OS fact computed now, and
deliberately **not** a claim about attentiveness.

### 7. `poreus doctor`

Every fact two parties can answer gets cross-checked, and any
disagreement is an error naming the disagreeing pair: poreus's
computed liveness against the session file's `status`; the stored host
name against the file's `name`; WAL size against sweep recency; open
threads against mailbox cursors; a `statusUpdatedAt` that has stopped
moving on a live pid.

That last check is this design's replacement for the stale-heartbeat
check — with the difference that the staleness is now the host's to
explain rather than ours to cause.

### 8. Schema v4 is a clean slate under a new filename

Per the ADR-0006/0009 posture, no migration. The store moves to
`db-v4.sqlite`, which makes the rollout window harmless by
construction: sessions still running the old binary keep writing
`db.sqlite` until they exit, rather than meeting a schema they cannot
read. The old store is snapshotted before cutover.

## Consequences

- **poreus keeps its reason to exist.** The host owns transport and
  idle wake; poreus owns typed endpoints, the registry, the reply duty
  and thread status, durability across restarts, and retention. This
  is a transport swap, not a retirement.
- **What this forbids.** No background thread may be added back
  without its own ADR. No stored heartbeat, and no stored copy of any
  OS or host fact. No payload on the doorbell. No retry of a doorbell.
  No error text that names a session address as a remedy. No presence
  filter on `discover`. No latency bound in the protocol.
- **Two naming systems coexist** — host session names and poreus role
  names — joined only by the lease in §5. Doctor owns the drift check.
- **A misdirected doorbell is a harmless no-op** in correctness terms:
  the woken session drains its own mailbox and finds nothing. It is not
  free, because it costs that session a turn, which is why exact
  targeting is required rather than optional.
- **`accept` masks the sender/receiver class asymmetry rather than
  resolving it.** Anyone who later removes `crossSessionInbound:
  accept` gets the holds back. The asymmetry itself is a host quirk
  neither zone can fix.

## Implementation notes

Recorded 2026-08-19, when the design above was built. Each of these
was a choice the design left open, not a change to it.

- **OQ-1 is settled: `doctor` is a CLI subcommand**, `poreus doctor`.
  The argument for an MCP tool was that reading host state needed a
  model in the loop to call `ListAgents`; the session file removed
  that, since `status` is readable from disk. An operator check
  belongs where an operator is looking, and the tool count stays at
  twelve. It exits non-zero on any disagreement, and it never repairs
  — a check that fixes things quietly is how drift becomes invisible
  again.

- **The force flag is named `create_role`**, on `request`, `call` and
  `notify`. Naming it after what it does rather than after its force
  semantics is deliberate: a model reading the schema should be able
  to tell that the flag creates something.

- **Replies route to the requester's role**, and only to the
  requester's session when the request carried no role. §4 says a
  session is "used for reply routing", which read literally would
  strand every late reply in a dead process's mailbox — the exact
  failure role mailboxes exist to remove. A reply is often hours
  behind its request; routing to the role means the successor reads
  the answer to work the role started. An unnamed sender has no
  successor, so its own mailbox is the only correct target.

- **`retire_name --force` discards only undelivered mail.** §4 asks
  for a discarded count; it does not say what happens to history.
  Deleting delivered messages would rewrite the record of work that
  actually happened, so force deletes only what is past the role's
  cursor — the messages that would otherwise be orphaned — and reports
  that count. The role's cursor row goes with it.

- **`cursors` lost its foreign key.** A role mailbox has no `sessions`
  row for a cascade to follow, so the sweep deletes orphaned cursors
  explicitly. Without that, every retired role and every swept session
  would leave a row behind forever.

- **A new `maintenance` table** holds one row, `last_sweep`. It is
  written *before* the sweep runs, so a sweep that throws still pushes
  the next attempt an hour out instead of retrying on every prompt.

- **`CanFileSystem` gained `getFileSize`**, so `doctor` can watch the
  write-ahead log grow without reading it.

- **Two pid namespaces, and the first `doctor` confused them.**
  `sessions.pid` is the pid of the `poreus serve` process; the host
  keys its session files by the pid of the *claude* process that
  spawned it. The version shipped in `e1c4cc6` compared one against
  the other, so both of its host comparisons were wrong on every real
  session: presence reported a false error for each live session, and
  the host-name drift check — the one this design added specifically —
  never fired at all. `host_sessions` already stores the join, keyed by
  the claude pid and carrying the session id, so the fix is a lookup
  rather than a new column.

  The tests missed it because the fixture published a session file
  under the serve pid as well as the claude pid, which the host never
  does. **A fixture more generous than reality tests nothing**; the
  regression test is the fixture corrected to publish only what the
  host publishes, and it fails five ways against the shipped code.

  Found by running `doctor` against the live fleet minutes after
  deploying it. Once fixed, the same run immediately reported a real
  drift: a session renamed to `kairos-hermes` whose stored lease still
  read `nixos-65`.

- **Never identify a session by the lease — only by the host file, read
  at the moment of printing.** The lease is a cache that goes stale
  between hook invocations, and a stale name is worse than no name in
  precisely the situation a name exists for: someone trying to find the
  right window. Measured on 2026-08-19: a `name-held` refusal printed
  the lease `nixos-65` for a session the user had renamed to
  `kairos-hermes`; a peer searched the host's live-session list for
  `nixos-65`, found nothing, and concluded the holder was dead. It was
  alive, one row above where the peer was looking, and it went on to
  pass `takeover` for a reason that was not true.

  That peer's own summary is the rule worth keeping: **a label that is
  wrong in a way that agrees with the reader's current guess is worse
  than one that is merely absent.** Doctor had the same defect in a
  sharper form — its `label` read the lease, so the finding whose whole
  job is to report that name as stale opened with it, while the correct
  name was bound on the same line.

  Two consequences, both implemented: the identifying name always comes
  from the host file; and a refusal never falls silent. "Held by a live
  session" with nothing after it reads as a formality, and the safe
  default then drifts towards passing `takeover` reflexively — so when
  the host name cannot be resolved the text says so explicitly, and the
  corrective action calls displacing a live holder a real decision.

- **The `presence` warn for a row with no serve pid is a property of a
  mixed-version fleet, not a defect.** During the v0.3→v0.4 window a
  session's hook writes the new store while its server still writes the
  old one, so every unmigrated peer trips that warn at once. It is true
  and it self-heals on restart; it should not be suppressed.

## Open questions

- **OQ-2.** Retention for a role mailbox whose holder never returns.
  Known roles only, so the mailbox is bounded by the registry, but the
  policy is unwritten. `doctor` reports the backlog; nothing trims it.
- **OQ-3.** Should the host map key off the session file's `sessionId`
  rather than the env variable, given L7? Not required for v0.4.
