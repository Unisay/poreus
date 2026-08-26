# ADR-0020: Identity never follows the host's current session id

## Status

Accepted — 2026-08-26. Closes ADR-0017 OQ-3 in the negative. Changes no
code; records a prohibition.

## Context

ADR-0017 left an open question: should the identity map key off the
`sessionId` in the host's own session file, rather than the id handed to
the process at spawn?

It was a fair question. ADR-0016 pins a claude process's poreus address
to the **first** id it ever presented, which means the address can
differ from the id the host is currently using for that window, and
every join on a session id becomes two-valued. That is untidy, and it
was reported as a defect on 2026-08-26 by the claude-config session,
which found the disagreement while chasing a doorbell bug and had to
reason it out from three ADRs to decide it was intended.

The question could not be settled without knowing what `/clear`
actually does. That evidence arrived the same day, from the `thema`
session, read out of the 2.1.245 binary and checked against 634 real
transcripts and one live experiment:

- `/clear` mints a plain `randomUUID` for the new session.
- `parentId` is set **in memory only** (`setCurrentAsParent`) and never
  reaches a transcript. The old file closes with no marker; the new file
  starts fresh. **There is no on-disk link forward.**
- `process.env.CLAUDE_CODE_SESSION_ID` *is* updated in place.
- `history.jsonl` records the clear as `{display, timestamp, project,
  sessionId}`, carrying the id of the session that *issued* it. The
  child's first record follows almost immediately: over 117 boundaries,
  median gap 0.0 s, p75 0.1 s, 106 of 117 inside a minute.

One of that report's premises also corrected an ADR: `/compact` does
**not** rotate the session id, contrary to ADR-0016's context paragraph.
Confirmed independently here over 48 transcripts carrying a compaction
marker — all 48 have turns before and after the marker in one file,
under one session id.

## Decision

**The identity map does not key off the host session file's
`sessionId`, and no delivery path joins on a session id.**

The reason is the missing link, not the untidiness. That field holds the
*post*-`/clear` id, and there is no `parentId` on disk to walk back
through. So keying identity off it would re-address a live session at
every `/clear` — a new address, a new mailbox, a role binding pointing
at the address the conversation just left. That is the exact failure
ADR-0016 exists to prevent, arrived at from the opposite direction: not
a rotation poreus failed to follow, but a rotation poreus chose to
follow and thereby split the mailbox mid-conversation.

Pinning to the process is right because the **process** is what a person
means by "the thema session". A `/clear` does not change the window, the
pid, or who is sitting in front of it.

The untidiness is handled where untidiness belongs: `doctor` reports the
disagreement at `ok`, naming ADR-0016, so the next reader closes the
question in one line instead of an investigation (ADR-0018 decision 4).

**What this forbids.** No consumer may resolve a session by comparing
its address to the host file's `sessionId`, and no consumer may treat a
disagreement between them as a fault. Nothing routes on the session id:
the mailbox belongs to the role (ADR-0017), and the host's name and
profile are both resolved through the process tree (ADR-0018, ADR-0019).

Rejected — **following the conversation through `history.jsonl`.** The
evidence is genuinely sufficient: a recorded `/clear` plus a child whose
first record lands inside the minute identifies the successor, and it
works for sessions nobody ever named, which is most of them. It is
rejected because poreus has no use for it. poreus routes to a mailbox
that already survives the boundary; reading the host's history to
rediscover a continuity it never lost would add a parser for
undocumented host state, a heuristic time window, and a second answer
to a question that already has one. A consumer that genuinely needs the
transcript chain — `thema` does — should build it there, not here.

Also worth noting for anyone tempted: counting `/clear` in
`history.jsonl` needs a whole-word match. `display == "/clear"` finds
323 entries; including a trailing space or an argument brings it to 438,
and the work profile alone holds 114 entries of `"/clear "`. And
`claude --print` writes nothing there at all.

## Consequences

- ADR-0017 OQ-3 is closed. No code changed.
- ADR-0016's decision stands, and is better supported than when it was
  written: the observed symptom (one claude process with two live
  `poreus serve` children under different spawn ids) was real, and
  pinning to the process is correct whatever causes a re-spawn.
- ADR-0016's *context* keeps one wrong premise — that compaction rotates
  the id. Left in place: accepted ADRs are appended to here, not
  rewritten, and the correction is recorded in ADR-0018 and above.
- A bare `/clear` carries the session name forward into the new
  transcript's first record; `/clear <name>` instead names the session
  being **left**. Neither affects poreus, which reads the name from the
  host's live session file rather than from any transcript. Recorded
  because it is the kind of asymmetry that looks like a bug when met
  cold.
