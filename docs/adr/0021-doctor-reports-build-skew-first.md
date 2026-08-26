# ADR-0021: Doctor reports build skew, and reports it first

## Status

Accepted — 2026-08-26. Adds one finding to `poreus doctor`. Suggested
twice by the `nixos` session before it was built.

## Context

Every `doctor` finding is a claim about **state**: what a row says, what
the host says, whether the two agree. Those hold whatever binary
anything is running, because doctor reads the database and the operating
system itself, not the servers.

The reader does not stop at state. An operator who sees `doctor` exit 0
concludes that delivery is healthy — and that is a claim about
**behaviour**, about the code inside the serving processes rather than
the code inside the CLI.

Those two can be different, and on this host they were. Measured
2026-08-26, minutes after deploying the ADR-0018 and ADR-0019 delivery
fixes:

- `doctor` from the new closure: exit 0, no error lines.
- All 10 live `poreus serve` processes: still the previous build.

So no session on the machine had the delivery fix in force — including
the session that had just shipped it — while the tool whose job is to
report on delivery health reported a clean bill. That is the same defect
shape as the three this ADR series has already fixed, one level up:
something read as healthy precisely where it was least trustworthy.

The check that would have caught it did not exist. The `nixos` session
proposed it twice before it was built, and the second time pointed out
what makes it different from an ordinary finding: skew is not one signal
among many, it is the **precondition** for generalising from any of the
others.

The information was free the whole time. `sessions.pid` is already
stored and already verified against the OS, and `/proc/<pid>/exe` on a
Nix host is a store path — so comparing two processes compares their
builds exactly, with no schema change and no new source of truth.

## Decision

1. **One aggregate `build` finding**, comparing every live serving
   process's executable against the CLI's own. One line naming the
   counts and the distinct builds, not one line per session: ten
   identical lines would bury the signal they carry.

2. **It sorts first.** The severity sort is stable, so placing it at the
   head of the finding list keeps it above other findings of equal
   severity. A finding that says whether the rest may be generalised
   from must not appear below them.

3. **`warn`, not `error`.** Skew is the normal state between a deploy
   and a session restart, it self-heals with no operator action, and
   nothing is wrong with the store. Making it an error would fail the
   exit code on every routine deploy, which is how a real error stops
   being read.

4. **Unreadable is stated, never assumed.** When the CLI cannot read its
   own executable the finding says so and makes no comparison. A silent
   `Nothing` would restore exactly the false clean bill this check
   exists to prevent.

5. **The wording separates the two kinds of claim explicitly**, because
   the whole failure was a reader generalising from one to the other:
   findings describe stored state and hold regardless; what they imply
   about delivery describes the CLI's code, which the listed processes
   are not running.

Rejected — **comparing version strings.** `poreus version` reads
`Paths_poreus`, which is the Cabal version, and it was `0.4.0.0` on both
sides of this deploy. A check that cannot distinguish the two builds it
exists to distinguish is worse than none: it would report agreement.

Rejected — **storing the build on the session row.** ADR-0017 §3. It is
a fact the OS will answer for, and a stored copy would be written when a
session is active and read when it is idle — the anti-correlation that
has now caused three separate defects.

Not attempted — **restarting servers, or telling the operator to.**
Doctor reports and never repairs (Note [What doctor is for]). Which
sessions are worth interrupting is a person's call.

## Consequences

- `CanSystemInfo` gains `getProcessExe`, its seventh capability and its
  fourth procfs reader.
- On this host the finding immediately fires: `10 of 10 live serve
  process(es) run a different build from this CLI`. Run from a cabal
  build tree it names the `dist-newstyle` path, which is the honest
  answer — it compares builds, not intentions.
- Exit code is unchanged by skew, so deploy automation that gates on
  `doctor` does not start failing.
- A latent asymmetry is now visible rather than assumed: the CLI can be
  older than the servers as easily as newer. The wording names both
  paths and does not claim which is which, because it cannot tell.

## Open questions

- Nothing tells a session that its own server is stale. The hook could
  compare at `SessionStart` and say so in its context line, which is the
  one moment a restart is free. Not built: it puts a diagnostic on the
  hot path, and the honest remedy — "restart this session" — is
  something only the person in front of it can weigh.
