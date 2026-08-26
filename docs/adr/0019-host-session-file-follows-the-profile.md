# ADR-0019: A session file is read from its own profile, not from ours

## Status

Accepted — 2026-08-26. Removes the last cross-profile blind spot in the
host-session lookup ADR-0018 rebuilt.

## Context

A host can run several Claude Code profiles. On this machine there are
two: `~/.claude-work` and `~/.claude-personal`. Neither sets
`POREUS_HOME`, so both resolve to the same default store. **One poreus
database, one role namespace, sessions from both profiles side by side
in `sessions`.**

Their session files do not share a directory. `hostSessionDir` read
`$CLAUDE_CONFIG_DIR` — the variable of whichever process happened to be
doing the reading — so a work-profile reader looked for a
personal-profile session's file in the work profile, found nothing, and
could not tell that apart from "the host publishes nothing for that
process".

Measured 2026-08-26, after ADR-0018 had already cut `poreus doctor` from
8 errors to 3: **all 3 survivors were this.** Three live sessions in
`/home/yura/Tomb2`, running under `.claude-personal`, whose files sat in
`~/.claude-personal/sessions/` the whole time. ADR-0018 first recorded
them as old host versions that publish nothing; that was wrong, and
version is provably not the cause — one of the three started *later*
than two publishing work-profile sessions on the same 2.1.245.

The doorbell was equally blind. No role was bound to a personal-profile
session at the time, so nothing was actually misdelivered — but the
store is shared, so one `claim_name` from the other profile would have
made it a live delivery defect with no warning.

`listHostSessions` made this structural rather than incidental: it
enumerated one directory, so no caller of it could ever see past the
profile it ran in.

## Decision

1. **The config dir is read from the target process.**
   `/proc/<pid>/environ` is readable for a process of the same user and
   holds `CLAUDE_CONFIG_DIR` (verified on all five live claude processes
   on this host). `hostSessionDirOf` reads it there;
   `readHostSession` goes through it, so every existing caller — the
   doorbell, `doctor`, `whoami`, the catalog's `holder_host_name` — is
   fixed at once.

   This is the same move as ADR-0018 decision 1, for the same reason:
   the value belongs to another process, the OS will hand it over, so
   derive it at the moment of need rather than storing a copy that goes
   stale silently.

2. **The environment is read as bytes and split on NUL before anything
   is decoded.** One undecodable neighbouring variable must not take the
   whole environment down with it, because that failure is silent and
   points the wrong way: an empty result reads as "this process has no
   such variable", which is indistinguishable from the variable being
   genuinely unset.

3. **The unreadable case falls back to our own profile.** A process that
   has exited, or a kernel with no procfs, gives nothing to read. Our
   own `CLAUDE_CONFIG_DIR` is then the best available guess and is
   exactly the pre-ADR-0019 behaviour, so the fallback can be no worse
   than what it replaces.

4. **`listHostSessions` is deleted.** Its only remaining caller was
   `doctor`, which enumerated a directory and then did `lookup pid`.
   Reading each pid's file directly is the same result, one profile-aware
   read per live session, and it removes the one function whose shape
   made a single-profile view unavoidable.

Rejected — **globbing `$HOME/.claude*/sessions/`.** It invents a naming
convention the host does not document, would break the moment a profile
lived anywhere else, and searching several directories for one pid
reintroduces the ambiguity ADR-0018 removed: pids are per-host, not
per-profile, so the same number can name a real file in the wrong
profile. Reading it would report another window's name as this
session's, which is the 2026-08-18 wrong-session failure with a new
cause.

Rejected — **storing the config dir on the session row.** It is an
exec-time fact of the claude process that the OS will answer for, so
ADR-0017 §3 applies. A stored copy would also be written on contact and
read when idle, which is the anti-correlation that has now caused three
separate defects (`sessions.host_name`, the pid map, and this).

## Consequences

- `poreus doctor` on this host: **3 errors to 0, exit code 0.** Every
  live session on the machine now resolves, in both profiles.
- Two of the newly visible sessions immediately produced a genuine
  `warn`: the host had not updated their status in 74 h and 51 h. That
  signal existed all along and was unreachable.
- Verified end-to-end against a `.backup` of the live store: a post to
  a personal-profile session's mailbox from a work-profile server
  returns `doorbell.agent = "majas-skateboard"`. It returned nothing
  before.
- `CanSystemInfo` gains `getProcessEnv`. It is the sixth capability on
  that class and the third that reads procfs, which is the right home
  for it — `CanEnv` is about our OWN environment and must stay that way.
- The `TestM` fake gains `tsProcEnv`, keyed by (pid, variable), beside
  `tsProcTable`. It is deliberately NOT a field on `ProcInfo`: that
  record is built positionally in 63 fixtures, and the churn would have
  buried the change under mechanical edits.

## Open questions

- Two poreus session rows can exist for one window. Measured
  2026-08-26: `s-bae95811` is addressed as itself while the host calls
  that window `2cd5d757`, and `s-2cd5d757` has its own row with no serve
  pid. So a hook contacted poreus under the post-`/clear` id without
  hitting the map. Harmless today — the mailbox stays with the pinned
  address and `doctor` warns about the pid-less row — but the identity
  chain is supposed to make this impossible, so the path that produced
  it is worth finding.
