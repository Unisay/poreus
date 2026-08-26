# ADR-0018: The claude pid comes from the process tree, not the map

## Status

Accepted — 2026-08-26. Corrects the doorbell resolution ADR-0017
specified; leaves ADR-0016's identity chain intact.

## Context

ADR-0017 deleted `sessions.host_name` because a stored copy of the
host's name for a session is renewed when the session is **active**,
while every consumer of it describes a session that is **idle**. The
renewal mechanism was anti-correlated with the need.

That reasoning was applied one level too shallow. The name was
demoted, and the replacement read the host's session file "joined
through `host_sessions`" — a second cache in the same path, written on
the same contact, with the same anti-correlation. Two defects, one
structure:

**The reverse lookup was single-valued over a many-valued map.**
`host_sessions` is keyed by (claude-ancestor pid, boot id, process
start time) — a *process instance*, deliberately (ADR-0016 §2). One
session id therefore carries a row per claude process that ever
presented it: `claude --resume` in a fresh window adds one, and so
does a reboot. The code did `lookup addr` over
`SELECT session_id, host_pid FROM host_sessions` with no `ORDER BY`,
which takes whichever row SQLite scans first — rowid order in
practice, so the **oldest** row won.

Measured on this host, 2026-08-26: 78 rows, 69 naming a pid that is no
longer alive, 6 session ids carrying more than one row. In all 6 the
first row was the dead one. Consequences, both live:

- `poreus doctor` reported **8 of 9** live, healthy, named sessions as
  `ERROR ... the host publishes no session file for its claude process
  <dead pid>`, and exited non-zero. The one cross-check whose job is
  to catch this drift was broken by it, and a check that cries wolf on
  every session is worse than no check at all.
- The doorbell was withheld from every one of those sessions.

**The map is refreshed only while the recipient is active.** The row
naming a window's pid is written when that window contacts poreus. The
doorbell exists to reach a window that is idle. Measured the same day:
a window started at 09:24:43 was unringable until 09:31:08, because
the row naming its pid did not exist yet. A post at 09:28:25 got no
doorbell, and ordering could not have helped — the correct row was
absent.

Found and reported by the claude-config session, which noticed it the
only way this is noticeable from outside: it delegated work twice to a
live, named, idle role, both posts came back with no doorbell, and a
hand-typed `SendMessage` to the same name woke the session instantly
both times.

## Decision

1. **The claude pid is derived, not stored.** The two pid namespaces
   are not two unrelated facts — the `poreus serve` process is a
   **child** of the claude process, so one hop up the parent chain
   crosses between them. `sessions.pid` is already checked against the
   OS on every read (`sessionLive`), so the whole join is available at
   the moment it is needed from state nobody has to keep fresh. This
   is ADR-0017 §3 ("never store a fact the OS or the host owns")
   applied to the layer that fix skipped.

2. **The map survives only as a fallback, and only in the one case
   the tree cannot answer**: a row no serving process ever wrote a pid
   into — a hook-only session, where there is no child to walk up
   from. That path is now scoped to the current boot, ordered
   `updated_at DESC`, and filtered to a pid the OS confirms alive.

3. **`doctor` resolves the pid exactly as the doorbell does**, so a
   finding describes the delivery path a peer will actually get. Its
   three host-lookup cases keep their distinct wording, with
   "unmapped" now meaning *no live claude process by either route*.

Rejected — **telling the sender that no ring channel exists.**
`doorbellFor` collapses four states into `Nothing`: role unheld,
holder row missing, holder not live, and holder live but unnameable.
The reporter argued the fourth is a *precondition* rather than an
*outcome*, and so is outside Note [The doorbell]'s rule that the
sender learns nothing it can branch on. The distinction is real, and
it is still rejected: the fourth state is a bug in poreus, not a fact
for a sender to route around, and publishing it would invite peers to
build fallback logic on top of a defect. Decisions 1–3 remove the
state instead. If it recurs, `doctor` is where it surfaces.

Not attempted — **matching a session to a window by workspace.** Two
live sessions shared one repository on 2026-08-18 and a workspace
match rang the wrong one (ADR-0017, L6).

## Consequences

- A live, named, idle session is ringable with no prior contact. The
  doorbell needs one `/proc` read per hop plus one file read, and no
  row.
- `poreus doctor` on this host drops from 8 errors to 3, and the 3 are
  real: live claude processes that publish no session file at all,
  started before the host version that writes them.
- Verified end-to-end against a copy of the live store, 2026-08-26: a
  post to the role `claude-config` returns
  `doorbell.agent = "persistent-sessions"`, the name that role's
  holder actually answers to. The same post returned no doorbell
  before the change.
- `hostNamesByAddress` (the catalog's `holder_host_name`) loses its
  one-pass optimisation and resolves per session. N is the number of
  live sessions on one host, so the cost is a few `/proc` reads; being
  wrong in one pass was not a saving.
- `host_sessions` keeps its ADR-0016 role — the identity chain — and
  is no longer read by anything that routes. Its staleness is now
  harmless rather than load-bearing.
- The fixture that made this invisible is fixed: `DoorbellSpec` gave
  the serve process no parent, so every doorbell test exercised the
  fallback and none exercised the path production uses.

## Open questions

- **ADR-0017 OQ-3 stays open, and is now less urgent.** Whether the
  identity map should key off the host session file's `sessionId`
  rather than the id handed to the process is unchanged as a question;
  what changed is that no delivery path joins on the session id any
  more, so getting it wrong no longer withholds a doorbell. Changing
  the key would re-address live sessions and split their mailboxes,
  which is the failure ADR-0016 exists to prevent, so it is not worth
  doing for tidiness.
- Three live claude processes on this host publish no session file.
  They are unringable and `doctor` calls it an error. Whether "the
  host is too old to publish" deserves an error or a warning is
  unsettled; it is not a poreus fault either way.
