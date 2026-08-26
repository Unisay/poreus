# poreus Protocol — Design

**Status:** v3, accepted 2026-08-19. Supersedes v2 (v0.3); the
functional requirements behind this contract are in
`docs/design/functional-spec-mcp.md`.
**Audience:** consumers of the MCP tool surface (agent hosts,
slash-command authors), and anyone integrating with the shared store.
**Companion:** `docs/adr/0001..0017-*.md` for the rationale behind
each design decision. ADR-0017 covers the v3 changes: mailboxes owned
by roles, no background threads, no stored liveness, no latency bound,
and the doorbell.

**What changed from v2, in one paragraph.** A mailbox belongs to a
role, not to a session, so a post to a role is queued whether or not a
process is serving it and the next holder drains the backlog. Liveness
is computed against the operating system on every read instead of
stored. The server has no background thread. `discover` has no
presence filter. The protocol states no latency bound; waking an idle
session is the host's job, and the sender rings once through the
host's own messaging.

---

## 1. Goals and non-goals

**Goals**

- Deliver structured messages between AI agent sessions on the same
  host, durably, with addressing and correlation.
- Zero-setup participation: a fresh session can send, receive, and
  reply with no registration step. Registration exists, adds value,
  and is optional.
- Make correct use the path of least resistance for an LLM agent:
  typed tool calls with schemas, guardrails at the point of error.
- Be small and learnable. The full protocol fits on one page.
- Stay agnostic to consumer policy. poreus does not know what a
  "task" is (ADR-0001).

**Non-goals**

- Task lifecycle as authoritative state. The derived thread status
  (§8) is a labeled convention-based projection, never a state
  machine.
- Multi-host delivery. Single-host SQLite is the model.
- Authentication. All sessions belong to one user on one machine;
  trust equals filesystem access.
- Reliability beyond durable local storage: no acknowledgement
  windows, no retries, no dead-letter queue.

---

## 2. Topology

One binary, four entry modes (ADR-0013/0017):

- **`poreus serve`** — the MCP server. The agent host spawns one per
  session over stdio; it owns the JSON-RPC loop **and nothing else**.
  There is no background thread, by decision rather than by omission
  (ADR-0017 §2): v0.3 forked one, it died unsupervised, and the three
  duties it carried failed silently for 45 hours.
- **`poreus hook`** — short-lived hook companion. Reads the host's
  hook record from stdin, claims the workspace role at `SessionStart`
  when it is free, delivers pending messages as context, sweeps
  retention at most hourly, always exits 0.
- **`poreus doctor`** — operator cross-check. Compares what poreus
  computed against what the operating system and the host say, prints
  one line per finding, exits non-zero on a disagreement. Reports;
  never repairs.
- **`poreus admin purge [--older-than DAYS]`**, **`poreus version`**
  — operator commands.

All processes share `$POREUS_HOME/db-v4.sqlite` (default
`$XDG_DATA_HOME/poreus`, fallback `~/.local/share/poreus`). There is
no daemon: any number of concurrent sessions, each with its own
server instance, over one store (WAL + `busy_timeout=10000`; every
read-modify-write runs in `BEGIN IMMEDIATE`). Writes are
traffic-driven only — 14 servers each writing a heartbeat every 5 s
produced lock storms against that 10 s timeout, so nothing writes
unless a message moves.

The store filename carries the schema generation. That is what makes a
rollout window harmless: a session still running the previous binary
keeps its own file rather than meeting a schema it cannot read.

Framing is newline-delimited JSON-RPC 2.0, one message per line, no
batching (ADR-0011). Bootstrap is implicit: every entry point applies
the (idempotent, `IF NOT EXISTS`) schema — no operation ever fails
with "not initialized".

## 3. Glossary

- **role** — short, unique, kebab-case identity (e.g. `nixos`)
  voluntarily claimed by a session; the durable principal. A role owns
  a **mailbox**, a **cursor**, and a **profile**, all of which outlive
  every process that holds it. Must not start with `s-`. Called a
  "name" in v2 and in the tool arguments, which kept their names.
- **session** — one agent session; the process that sends, receives,
  and serves a role. Ephemeral.
- **session address** — `s-<session id>`. Auto-provisioned at first
  contact, stable across resumes, never typed by hand. A session has
  its own mailbox, used for replies to unnamed senders and for peers
  addressing a specific process. It dies with the process.
- **mailbox** — where a message is stored and from where it is
  drained, keyed by a role name or a session address. The two spaces
  cannot collide: a role may not start with `s-`.
- **binding** — the claim linking one session to a role. At most one
  session per role. A new holder inherits the role's mailbox and its
  cursor, so it reads exactly what its predecessor did not.
- **profile** — summary, tags, endpoints attached to a *role*;
  survives sessions.
- **endpoint** — a typed capability: `verb` (kebab-case, unique per
  name), `description`, `autonomy` (`auto` | `confirm`), optional
  free-text `usage_hint`. No argument schemas (ADR-0015 territory:
  LLM callers read descriptions).
- **message** — the atomic delivery unit; flat record, immutable once
  posted (§5).
- **kind** — `request` or `notice`; the protocol's only
  classification (ADR-0002). A reply is a notice with non-null
  `in_reply_to`.
- **thread** — a request plus every notice whose `in_reply_to` points
  at it. Derived, not stored.
- **cursor** — per-mailbox high-water mark (`seq`) of messages
  already delivered through acknowledged paths. Snapshots never touch
  it (ADR-0005 semantics, carried forward).
- **doorbell** — a payload-free poke the *sending model* may send once
  through the host's own `SendMessage`, to wake an idle recipient
  sooner. Not part of delivery; see §7.

## 4. Identity (REG)

Every session is addressable from its first contact — no
registration. The server **and the hook** resolve the session id
through one shared chain (ADR-0016; the host rotates session ids
across compactions and re-spawns servers with fresh ids while the
original connection keeps serving, so the two sides must converge by
construction):

1. `$POREUS_SESSION_ID` — explicit override (tests, future-proofing);
   bypasses the map.
2. The `host_sessions` map keyed by (claude-ancestor pid, boot id,
   process start time) — authoritative for a running claude process
   once seeded; later id rotations are deliberately ignored.
3. The host-provided id (`$CLAUDE_CODE_SESSION_ID` for the server —
   observed, not documented; the stdin `session_id` for the hook),
   seeding the map at first contact.
4. A freshly minted id, seeding the map.

Address = `s-` + session id. Workspace = `$CLAUDE_PROJECT_DIR`, else
the repo root of the server's cwd. host_sessions is a disposable
cache: a shape change drops and recreates it (`migrate`), never
migrates it.

**Claiming a role** is idempotent for the holder. A role held by a
live session refuses the claim (`name-held`) unless the caller passes
takeover; a role whose holder's process is gone is claimable directly.
One role per session: a new claim releases the previous one. Releasing
(explicitly, or by session end) keeps the role, its profile, its
mailbox and its cursor for the next claimant — nothing is lost by
releasing. Retiring deletes the role outright, and **refuses while
undelivered mail is queued for it**; `force: true` retires anyway and
reports the discarded count. Already-delivered history survives a
retire.

**No error text ever names a session address as a remedy.** A
`name-held` refusal names the role and, when the host published one,
the holder's host session name — enough for a person to find the
window, and not an address a peer can post to. v0.3 handed addresses
out as hints; a peer learned the habit and two days later misrouted a
request to a session that merely shared a workspace (ADR-0017, L5/L6).

**Liveness** is the triple `(pid, boot_id, proc_start)`, compared
against the operating system on every read. Nothing about it is
stored, because v0.3 stored a heartbeat and it lied for 45 hours after
the thread writing it died. A pid plus a boot id cannot detect PID
reuse and its failure direction is *false alive*, so the process start
time is part of the triple.

The word is deliberately narrow: it reports that a process **exists**,
not that anyone is reading. A wedged session reads alive. Waking a
session is the host's job now, and poreus only promises to queue, so
the narrower fact is the one it can own. A row whose serving process
never identified itself (the hook creates such rows) reads alive;
`poreus doctor` is what flags the residual case.

**Automatic role claim.** The hook claims the workspace-derived role
(repo basename, or `.poreus/alias`) at `SessionStart` when it is free
or its holder's process is gone, and says so in one context line. A
live holder is left alone — parallel topic sessions in one repo stay
legitimately nameless. This replaces v0.3's suggestion, which existed
because every `--resume` orphaned the binding and a human had to
notice. A tool result still carries a `session-unnamed` warning when
the session holds no role and one is available, for hosts with no hook
installed.

**The host's name for a session is never stored.** Wherever poreus
needs it — the doorbell target (§7), the catalog's `holder_host_name`,
`whoami`, a refusal saying which window holds a role — it is read from
`$CLAUDE_CONFIG_DIR/sessions/<claude-pid>.json` at that moment.

**Nor is the claude pid it is keyed by.** The `poreus serve` process is
a child of the claude process, and `sessions.pid` is checked against the
operating system on every read, so one hop up the parent chain gives the
claude pid with nothing cached (ADR-0018). `host_sessions` answers only
for a row no serving process ever wrote a pid into — a hook-only
session, where there is no child to walk up from — and that path is
scoped to the current boot, ordered newest-first, and filtered to a pid
the OS confirms alive.

**Nor the directory it lives in.** A host can run several Claude Code
profiles that share one poreus store, and their session files do not
share a directory, so the file is read from the *target* process's
`CLAUDE_CONFIG_DIR` — taken from `/proc/<claude-pid>/environ` — and not
from the reader's (ADR-0019). Reading the reader's own profile made
`doctor` call three live sessions broken while their files sat one
directory over. When that environment cannot be read, the reader's own
profile is the fallback.

**Identity never follows the host's current session id.** `/clear`
mints a fresh id in the same process and writes no parent link to disk,
so keying identity off the host file's `sessionId` would re-address a
live session and split its mailbox mid-conversation (ADR-0020). The
address stays pinned to the process; `doctor` reports the resulting
disagreement at `ok` and nothing routes on it.

A stored copy was tried and removed, twice, for one reason. It was
renewed when a session made a poreus call or a hook fired — that is,
when the session was **active** — while every consumer of it describes a
session that is **idle**. The renewal was anti-correlated with the need,
so the value was least trustworthy exactly where it was used: measured
2026-08-19, the two sessions carrying stale names were both idle, both
resumed under a new name, and both were the ones worth ringing. The
first fix demoted the name and kept routing on the pid cache underneath
it, which had the same shape and the same anti-correlation: measured
2026-08-26, it withheld the doorbell from every live named session on
the host and made `poreus doctor` report 8 of 9 of them as broken.

## 5. Message record

Flat, immutable once posted. Server-assigned: `seq` (the total order
and cursor key, ADR-0012), `message_id`
(`YYYYMMDD-HHmmss-<tag>-<4hex>`, tag = sender's name or session-id
fragment), `from`/`from_name`, `created_at` (UTC, ms precision, for
display/`since`/retention only — ordering is `seq`).

```json
{
  "message_id": "20260814-120301-folios-a1b2",
  "from": "s-1f0c…",            // sending session address
  "from_name": "folios",        // sender's role, or null
  "to": "nixos",                // the recipient MAILBOX key
  "to_kind": "role",            // "role" | "session"
  "kind": "request",            // "request" | "notice"
  "in_reply_to": null,          // message_id correlation (notices)
  "payload": { … },             // opaque; stored verbatim, never interpreted
  "created_at": "2026-08-14T12:03:01.123Z"
}
```

`to` + `to_kind` replace v2's `to` / `to_name` pair. There is no
"resolved session address" any more: a message addressed to a role
belongs to the role, and the process that reads it is whichever one
holds the role at read time.

Payload conventions written by the typed tools (consumers may extend;
the store never validates):

- request: `{"request_kind":"freetext","description":…,
  "expected_outcome":…,"data":…}`
- call: `{"request_kind":"rpc","verb":…,"args":{…}}`
- reply: `{"event":…,"summary":…,"artifacts":[…]}`
- notify: `{"event":…,"summary":…,"data":…}`

Lifecycle vocabulary is recommended, not enforced (ADR-0007):
`started`, `stuck`, `completed`, `failed`, `aborted`; the last three
are terminal. Unknown events are legal.

## 6. Send-time resolution (SEND-5)

`to` accepts a role name or a session address (distinguished by the
`s-` prefix). A role resolves to that role's mailbox — written to
whether or not a session is serving it right now.

**Known roles queue; unknown names fail.** A peer restarting is not an
error condition for the sender, so a role that exists takes mail. A
name that was never claimed still fails, because a typo would
otherwise create a mailbox nobody will ever drain and the sender would
wait forever on a request that reached no one. `create_role: true`
lets a sender queue for a role deliberately — seeding work for a
session that does not exist yet — so the choice belongs to the sender
rather than to poreus guessing. This reverses ADR-0012's fail-fast
rule; see ADR-0017 §4.

| `to` | Outcome |
|---|---|
| role, held by a live process | stored, no warning |
| role, held by a dead process, or unheld | stored, warning `role-unheld` |
| name that was never claimed | `unknown-recipient` |
| same, with `create_role: true` | role created, stored, warning `role-created` |
| session address, process alive | stored, no warning |
| session address, process gone | stored, warning `recipient-process-gone` |
| unknown session address | `unknown-recipient` |

**Replies route to the requester's role** when the request carried
one, and to the requester's own session only when it held none. A
reply is often hours behind its request, by which time the asking
process may be gone; routing to the role means its successor reads the
answer to work the role started.

`from`, `from_name`, `message_id`, `created_at` are server-assigned;
the caller can forge none of them.

## 7. Delivery (RECV)

Attendance is automatic: it begins with the session's first contact
and lasts for its lifetime. There is no watch command and nothing to
forget to start. A session drains **two mailboxes** — its own and its
role's — merged back into `seq` order, which is the total order across
both.

Two layers, and only the first makes a promise (ADR-0017 §1).

**Layer 1 — the ledger.** The system of record, with two acknowledged
paths:

1. **Piggyback**: every successful tool result carries `new_messages`
   — everything past each cursor, in `seq` order; the cursors advance
   in the same `BEGIN IMMEDIATE` transaction. Delivered requests carry
   a `reply_duty` reminder.
2. **Hook**: `poreus hook` on `SessionStart` / `UserPromptSubmit`
   prints a context digest (other events use the
   `hookSpecificOutput.additionalContext` envelope) and advances the
   cursors identically. Silent when nothing is pending and nothing was
   claimed.

Exactly-once holds per acknowledged stream — the transaction makes
server and hook mutually safe.

**Layer 2 — the doorbell.** Latency only. After a post to a mailbox
whose holder is reachable, the result carries a `doorbell` object
naming the recipient's **host session name** and a fixed ~100-byte
body. The *sending model* may ring it once through the host's own
`SendMessage` tool.

Rules, stated in `instructions` because the model holds the tool:

- **Ring once. Never retry.** A retried poke turns a latency
  optimisation into a denial of service against a person's attention.
- **Never wait on it, never branch on it.** The post already
  succeeded.
- **Never put content in it.** The native transport has a size
  ceiling; a held poke parks a second copy of the message where
  neither side controls it; and an own-child socket write arrives
  framed as `role: user`, indistinguishable from something the human
  typed.

A `SendMessage`-initiated turn fires `UserPromptSubmit`, so the ring
wakes the session *and* the hook drains the mailbox in the same turn,
through the acknowledged path. One round trip, not two.

**poreus states no latency bound.** v2's "≤ 5 s while active" is
withdrawn. The doorbell can be held, refused, or lost to a permission
mismatch, and the sender learns this only out of band, in text it
cannot branch on. The guarantee is: **a message is delivered at the
recipient's next prompt or tool call.** Nothing sooner is promised.

**Catch-up and adoption.** A resumed session (same id → same address)
continues from its cursor. Work its predecessor never read is in the
role's mailbox, and the role's cursor came with the role, so the
successor simply reads it. v2 needed `adoption: true` for this; the
flag is gone rather than deprecated, because a flag that silently does
nothing is worse than one that fails.

## 8. Tool surface

Twelve tools (host-prefixed as `mcp__poreus__*`). All inputs are
schema-checked objects; all outputs carry both a compact `text`
content block and `structuredContent`.

| Tool | Input | Output (structuredContent) |
|---|---|---|
| `whoami` | `{}` | `{address, name?, workspace, host_name?}` |
| `claim_name` | `{name?, takeover?}` | `{name, previous_holder?, released?}` |
| `release_name` | `{}` | `{released}` |
| `retire_name` | `{name, force?}` | `{retired, open_requests, discarded}` |
| `publish_profile` | `{name?, summary, tags?, endpoints?[{verb, description, autonomy, usage_hint?}]}` | `{name, endpoints, updated_at, …}` |
| `discover` | `{tag?, verb?, address?}` | `{names[], sessions[]}` with presence annotations |
| `request` | `{to, description, expected_outcome?, payload?, create_role?}` | `{message, doorbell?}` |
| `call` | `{to, verb, args?, create_role?}` | `{message, doorbell?}` (+ warning if endpoint unknown) |
| `reply` | `{in_reply_to, event, summary?, artifacts?}` | `{message, doorbell?}` (+ warning if thread already terminal) |
| `notify` | `{to, event?, summary?, payload?, create_role?}` | `{message, doorbell?}` |
| `messages` | `{scope: inbox\|open\|history\|thread, thread?, from?, involving?, kind?, since?, limit?}` | `{messages[], thread_status?}` |
| `purge` | `{older_than_days?, confirm: true}` | sweep counts |

Every successful result may additionally carry `warnings[]` and
`new_messages[]` (§7). The `messages` tool is the one query surface,
and it reads **both** mailboxes the session drains: `inbox` (addressed
to me, filters compose), `open` (requests to me with no reply notice
from anyone — including ones a former holder of my role left
unanswered), `history` (recent traffic involving a role or address,
default me, newest first, limit 10), `thread`
(root + replies, chronological, plus the derived `thread_status`:
`open` / `active` / `terminal` with the terminal notice — a labeled
convention projection, recomputed on read, never stored, never an
input to any other behavior).

**`discover` has no presence filter.** Presence is an annotation:
`holder_process: alive|dead|null` on a role, `process: alive|dead` on
a session, plus `queued` (undelivered count) and `holder_host_name`.
A filter turns a wrong presence reading into a wrong routing decision:
`live_only: true` returned an empty list on 2026-08-18 while a named
session was serving, the caller read that as "no such role", and
guessed a session by workspace — picking the wrong one of two sharing
a repo. An annotation leaves the routing decision on the role.

The server's `initialize` result carries `instructions` with the
reply duty (POL-1): **every received request gets exactly one
terminal notice (`completed`/`failed`/`aborted`) with a summary;
`started` when work is more than momentary; `stuck` when blocked** —
plus the addressing rule (roles, not sessions) and the doorbell rules
(§7). Nothing is negotiated per request — `subscribe` is gone
(ADR-0015).

## 9. Errors and warnings

Domain failures are tool-level results (`isError: true`) carrying
`{code, message, action?}`; JSON-RPC errors are reserved for
transport/shape failures (parse error, unknown method, unknown tool).
Exit codes are not an API.

| Code | Condition |
|---|---|
| `invalid-input` | structurally invalid arguments |
| `unknown-agent` | query targets an address absent from the catalog |
| `unknown-recipient` | post to a never-claimed name or unknown session address |
| `name-unbound` | *(retired in v3 — a known role queues instead)* |
| `unknown-message` | correlation/thread id does not exist |
| `name-held` | claim refused: role held by a live session |
| `storage-failure` | durable store unavailable or corrupt |
| `internal` | anything else |

`retire_name` on a role with queued mail is an `invalid-input` whose
`action` names `force: true`.

Warnings (non-blocking, on successful results):

| Warning | Meaning |
|---|---|
| `role-unheld` | queued; no session holds the role right now |
| `role-created` | the role did not exist and `create_role` made it |
| `recipient-process-gone` | the addressed *session's* process is gone; address the role instead |
| `endpoint-not-found` | no such verb in the role's published profile |
| `thread-already-terminal` | the thread already has a terminal notice |
| `session-unnamed` | this session holds no role and one is available |

**No error or warning text names a session address as a remedy**
(ADR-0017, L5). v2 enriched resolution failures with a workspace hint
carrying a live session's address; that hint is removed, and the
refusal names only the role.

## 10. Storage schema (v4)

All DDL `IF NOT EXISTS`; no `schema_version` table (ADR-0009 posture,
re-adopted by ADR-0017 for the v4 clean slate under a new filename).

```sql
sessions(address PK, workspace, pid, boot_id, proc_start,
         first_seen_at, last_seen_at, ended_at) -- last_seen_at: retention only
cursors(mailbox PK, last_seq)                   -- no FK: a role has no session row
names(name PK, summary, tags /*JSON*/, bound_session → sessions
      ON DELETE SET NULL, bound_at, created_at, profile_updated_at)
endpoints(name → names ON DELETE CASCADE, verb, description,
          autonomy CHECK IN ('auto','confirm'), usage_hint,
          PK (name, verb))
messages(seq INTEGER PK AUTOINCREMENT,          -- total order + cursor key
         id UNIQUE, from_address, from_name,
         to_mailbox, to_kind CHECK IN ('role','session'),  -- the delivery key
         kind CHECK IN ('request','notice'), in_reply_to,
         payload /*verbatim*/, created_at)
host_sessions(host_pid, boot_id, proc_start, session_id, workspace,
              updated_at, PK (host_pid, boot_id, proc_start))
                                    -- authoritative identity map (ADR-0016);
                                    -- one row per process INSTANCE, so a
                                    -- session id carries several. Nothing
                                    -- routes on it (ADR-0018).
maintenance(key PK, value)          -- currently one row: last_sweep
```

Indexes: `(to_mailbox, seq)`, `(from_address, seq)`, `in_reply_to`,
`created_at`.

`cursors` has no foreign key on purpose: a role mailbox has no
`sessions` row for a cascade to follow, and a session mailbox must not
lose its cursor before retention says so. The sweep deletes orphaned
cursors instead.

## 11. Retention (MAINT-1)

One age-based window (default **30 days**, `POREUS_RETENTION_DAYS`
override) governs everything ephemeral: messages, ended/stale
sessions' records (name bindings reset to NULL), stale identity
mappings, and cursors whose mailbox no longer exists in either
namespace. Roles and profiles are never swept — only explicitly
retired.

**The sweep runs on the hook path**, at most hourly, guarded by a
`last_sweep` row. It used to run on the server's tick; when that
thread died the sweep stopped with it, and the first visible symptom
was a 4.1 MB write-ahead log, days later. `poreus admin purge` and the
`purge` tool trim earlier on demand, and `poreus doctor` reports both
the sweep age and the log size.

## 12. Versioning posture

Clean slate again at the v3 cutover: no data migration from v0.3. The
store moves to `db-v4.sqlite`, so the rollout window is harmless by
construction — sessions still running the old binary keep writing
`db.sqlite` until they exit. Peers re-claim their roles on their next
visit, and the hook does it for them (ADR-0006/0009, re-adopted by
ADR-0017 §8). Afterwards: additive evolution preferred; any
non-additive change requires an ADR and an update to this document.

**What v3 forbids**, so a future change has to argue against it
rather than around it: no background thread without its own ADR; no
stored copy of any fact the OS or the host owns; no payload on the
doorbell; no retry of a doorbell; no error text naming a session
address as a remedy; no presence filter on `discover`; no latency
bound in this document.
