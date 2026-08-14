# poreus Protocol — Design

**Status:** v2, accepted 2026-08-14. Supersedes v1 (the v0.2 CLI
surface) in full; the functional requirements behind this contract are
in `docs/design/functional-spec-mcp.md`.
**Audience:** consumers of the MCP tool surface (agent hosts,
slash-command authors), and anyone integrating with the shared store.
**Companion:** `docs/adr/0001..0015-*.md` for the rationale behind
each design decision. ADR-0010..0015 cover the v2 pivot.

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

One binary, three entry modes (ADR-0013):

- **`poreus serve`** — the MCP server. The agent host spawns one per
  session over stdio; it owns the JSON-RPC loop and a 5-second tick
  thread (heartbeat, channel push, hourly retention sweep).
- **`poreus hook`** — short-lived hook companion. Reads the host's
  hook record from stdin, delivers pending messages as context,
  always exits 0.
- **`poreus admin purge [--older-than DAYS]`**, **`poreus version`**
  — operator commands.

All processes share `$POREUS_HOME/db.sqlite` (default
`$XDG_DATA_HOME/poreus`, fallback `~/.local/share/poreus`). There is
no daemon: any number of concurrent sessions, each with its own
server instance, over one store (WAL + `busy_timeout=10000`; every
read-modify-write runs in `BEGIN IMMEDIATE`).

Framing is newline-delimited JSON-RPC 2.0, one message per line, no
batching (ADR-0011). Bootstrap is implicit: every entry point applies
the (idempotent, `IF NOT EXISTS`) schema — no operation ever fails
with "not initialized".

## 3. Glossary

- **session** — one agent session; the unit that sends, receives,
  and attends. Owns exactly one **mailbox**.
- **session address** — `s-<session id>`; the sole delivery key
  (ADR-0012). Auto-provisioned at first contact, stable across
  resumes, never typed by hand.
- **name** — short, unique, kebab-case identity (e.g. `nixos`)
  voluntarily claimed by a session. Pure send-time resolution layer:
  names have no mailbox. Must not start with `s-`.
- **binding** — the claim linking one live session to a name. At most
  one session per name. Rebinding never reroutes stored messages.
- **profile** — summary, tags, endpoints attached to a *name*;
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
- **cursor** — per-session high-water mark (`seq`) of messages
  already delivered through acknowledged paths. Snapshots never touch
  it (ADR-0005 semantics, carried forward).

## 4. Identity (REG)

Every session is addressable from its first contact — no
registration. The server resolves the session id through this chain
(ADR-0014):

1. `$POREUS_SESSION_ID` — explicit override (tests, future-proofing).
2. `$CLAUDE_CODE_SESSION_ID` — the host's session id (observed, not
   documented; never a single point of failure). The hook receives
   `session_id` on stdin (documented), so hook and server agree.
3. The `host_sessions` map keyed by (claude-ancestor pid, boot id) —
   a respawned server inside the same host session reuses its id.
4. A freshly minted id, persisted into `host_sessions`.

Address = `s-` + session id. Workspace = `$CLAUDE_PROJECT_DIR`, else
the repo root of the server's cwd.

**Claiming a name** is optional and idempotent for the holder. A name
bound to a live session refuses the claim (`name-held`, identifying
the holder) unless the caller passes takeover; a dead holder's name is
claimable directly. One name per session: a new claim releases the
previous one. Releasing (explicitly, or by session end) keeps the name
and its profile for the next claimant. Retiring deletes the name
outright and surfaces the count of open requests addressed to it.

**Liveness**: a session is live iff not ended, its serving pid (with
matching boot id) is alive when known, and its heartbeat is within
15 s (the tick beats every 5 s). Presence is visible in the catalog.

## 5. Message record

Flat, immutable once posted. Server-assigned: `seq` (the total order
and cursor key, ADR-0012), `message_id`
(`YYYYMMDD-HHmmss-<tag>-<4hex>`, tag = sender's name or session-id
fragment), `from`/`from_name`, `created_at` (UTC, ms precision, for
display/`since`/retention only — ordering is `seq`).

```json
{
  "message_id": "20260814-120301-folios-a1b2",
  "from": "s-1f0c…",            // sender session address (delivery key)
  "to": "s-9d2e…",              // recipient session address (delivery key)
  "from_name": "folios",        // annotation: sender's bound name, or null
  "to_name": "nixos",           // annotation: the as-written designator, or null
  "kind": "request",            // "request" | "notice"
  "in_reply_to": null,          // message_id correlation (notices)
  "payload": { … },             // opaque; stored verbatim, never interpreted
  "created_at": "2026-08-14T12:03:01.123Z"
}
```

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

`to` accepts a name or a session address (distinguished by the `s-`
prefix). Names resolve **at post time** to the session currently
bound; the message is stored and delivered keyed by that session
address, with the name preserved as the `to_name` annotation. Later
rebinding never reroutes stored messages.

Resolution failures are immediate and explicit:

- never-claimed name → `unknown-recipient`
- claimed name, no live bound session → `name-unbound` (fail fast; no
  store-and-forward to absent roles — check presence via `discover`)
- unknown session address → `unknown-recipient`
- ended session's address → **accepted with warning**
  `recipient-session-ended` (the send/session-end race makes
  rejection wrong; the mailbox persists until retention)

`from`, `from_name`, `message_id`, `created_at` are server-assigned;
the caller can forge none of them.

## 7. Delivery (RECV)

Attendance is automatic: it begins with the session's first contact
and lasts for its lifetime. There is no watch command and nothing to
forget to start. Three delivery paths (ADR-0014):

1. **Piggyback** (acknowledged): every successful tool result carries
   `new_messages` — everything in the mailbox past the cursor, in
   `seq` order; the cursor advances in the same `BEGIN IMMEDIATE`
   transaction. Delivered requests carry a `reply_duty` reminder.
2. **Hook** (acknowledged): `poreus hook` on `SessionStart` /
   `UserPromptSubmit` prints a context digest (other events use the
   `hookSpecificOutput.additionalContext` envelope); advances the
   cursor identically. Silent when nothing is pending.
3. **Channel push** (best-effort, unacknowledged): the server tick
   emits `notifications/claude/channel` frames for messages no
   acknowledged path has delivered. **Never advances the cursor**; the
   server tracks its own pushed floor. A rare channel-then-piggyback
   duplicate is possible — consumers deduplicate by `message_id`.
   Requires a host that surfaces channel notifications (research
   preview; see ADR-0014).

Exactly-once holds per acknowledged stream (the transaction makes
server and hook mutually safe). Latency: ≤ 5 s while the session
interacts or when channel push is live; otherwise pending messages
arrive at the session's next interaction.

**Catch-up:** a resumed session (same id → same address) continues
from its cursor. A mailbox has no predecessor by construction; work
stranded in a dead session's mailbox is recovered by **adoption**:
`messages scope: open, adoption: true` (while bound to the name)
lists requests whose `to_name` is yours but whose target session no
longer represents it — adopting is simply replying (correlation is by
message id).

## 8. Tool surface

Twelve tools (host-prefixed as `mcp__poreus__*`). All inputs are
schema-checked objects; all outputs carry both a compact `text`
content block and `structuredContent`.

| Tool | Input | Output (structuredContent) |
|---|---|---|
| `whoami` | `{}` | `{address, name?, workspace}` |
| `claim_name` | `{name?, takeover?}` | `{name, previous_holder?, released?}` |
| `release_name` | `{}` | `{released}` |
| `retire_name` | `{name}` | `{retired, open_requests}` |
| `publish_profile` | `{name?, summary, tags?, endpoints?[{verb, description, autonomy, usage_hint?}]}` | `{name, endpoints, updated_at, …}` |
| `discover` | `{tag?, verb?, address?, live_only?}` | `{names[], sessions[]}` with liveness/bindings |
| `request` | `{to, description, expected_outcome?, payload?}` | `{message}` |
| `call` | `{to, verb, args?}` | `{message}` (+ warning if endpoint unknown) |
| `reply` | `{in_reply_to, event, summary?, artifacts?}` | `{message}` (+ warning if thread already terminal) |
| `notify` | `{to, event?, summary?, payload?}` | `{message}` |
| `messages` | `{scope: inbox\|open\|history\|thread, thread?, from?, involving?, kind?, since?, limit?, adoption?}` | `{messages[], thread_status?}` |
| `purge` | `{older_than_days?, confirm: true}` | sweep counts |

Every successful result may additionally carry `warnings[]` and
`new_messages[]` (§7). The `messages` tool is the one query surface:
`inbox` (to me, filters compose), `open` (requests with no reply
notice from anyone; adoption scope per §7), `history` (recent traffic
involving an address, default me, newest first, limit 10), `thread`
(root + replies, chronological, plus the derived `thread_status`:
`open` / `active` / `terminal` with the terminal notice — a labeled
convention projection, recomputed on read, never stored, never an
input to any other behavior).

The server's `initialize` result carries `instructions` with the
reply duty (POL-1): **every received request gets exactly one
terminal notice (`completed`/`failed`/`aborted`) with a summary;
`started` when work is more than momentary; `stuck` when blocked.**
Nothing is negotiated per request — `subscribe` is gone (ADR-0015).

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
| `name-unbound` | post to a claimed name with no live bound session |
| `unknown-message` | correlation/thread id does not exist |
| `name-held` | claim refused: name bound to another live session |
| `storage-failure` | durable store unavailable or corrupt |
| `internal` | anything else |

Warnings (non-blocking, on successful results): `endpoint-not-found`,
`thread-already-terminal`, `recipient-session-ended`.

## 10. Storage schema (v3)

All DDL `IF NOT EXISTS`; no `schema_version` table (ADR-0009 posture,
re-adopted by ADR-0012 for the v3 clean slate).

```sql
sessions(address PK, workspace, pid, boot_id,
         first_seen_at, heartbeat_at, ended_at)
cursors(session_address PK → sessions ON DELETE CASCADE, last_seq)
names(name PK, summary, tags /*JSON*/, bound_session → sessions
      ON DELETE SET NULL, bound_at, created_at, profile_updated_at)
endpoints(name → names ON DELETE CASCADE, verb, description,
          autonomy CHECK IN ('auto','confirm'), usage_hint,
          PK (name, verb))
messages(seq INTEGER PK AUTOINCREMENT,          -- total order + cursor key
         id UNIQUE, from_address, to_address,   -- delivery keys
         from_name, to_name,                    -- annotations
         kind CHECK IN ('request','notice'), in_reply_to,
         payload /*verbatim*/, created_at)
host_sessions(host_pid, boot_id, session_id, workspace, updated_at,
              PK (host_pid, boot_id))           -- identity fallback map
```

Indexes: `(to_address, seq)`, `(from_address, seq)`, `in_reply_to`,
`to_name`, `created_at`.

## 11. Retention (MAINT-1)

One age-based window (default **30 days**, `POREUS_RETENTION_DAYS`
override) governs everything ephemeral: messages, ended/stale
sessions' records (cursors cascade; name bindings reset to NULL).
Names and profiles are never swept — only explicitly retired. The
sweep runs at server start and hourly; `poreus admin purge` /
the `purge` tool trim earlier on demand.

## 12. Versioning posture

Clean slate at the v2 cutover: no data migration from v0.2; the store
is recreated empty and peers re-claim names on their next visit
(ADR-0006/0009, re-affirmed by ADR-0010). Afterwards: additive
evolution preferred; any non-additive change requires an ADR and an
update to this document.
