# ADR-0016: The host map is the authoritative session identity

## Status

Accepted — 2026-08-15. Inverts the identity-chain precedence of
ADR-0014.

## Context

ADR-0014's chain trusted the host-provided id first: override →
`$CLAUDE_CODE_SESSION_ID` → host_sessions map → mint. The map was a
fallback for servers spawned with no id at all.

Production falsified the premise that the host hands every process the
same id (found and root-caused by the claude-config agent, and
reproduced live in the poreus session itself). Claude Code rotates the
session id across compactions and re-spawns MCP servers with the fresh
id — while the *original* connection keeps serving tool calls under
the id it was spawned with. Meanwhile hooks always receive the
*current* id on stdin. Result: the serving process and the hook derive
different addresses; mail (and the name binding) accumulates at the
server's address while the hook silently drains an empty mailbox.
Observed on this host: one claude process with two live `poreus serve`
children under different spawn ids, six servers host-wide with
heterogeneous ids, and a reply that sat undelivered for two hours
until a tool call's piggyback flushed it.

A second, independent defect hid the first: the claude-ancestor walk
matched process names by the bare prefix `claude`, but on NixOS the
wrapped binary's comm is `.claude-unwrapp` (wrapProgram rename +
15-char comm truncation) — the walk never matched, so the map-recovery
path had never actually run on this host.

## Decision

1. **The host map wins.** The shared chain — used identically by the
   server and the hook (`resolveIdentityFrom`) — is: `$POREUS_SESSION_ID`
   override (bypasses the map) → **host_sessions row for this claude
   process** → host-provided id (env for the server, stdin `session_id`
   for the hook), seeding the map → minted id, seeding the map. The
   first contact of a claude process stamps its identity for the
   process's whole life; later id rotations are deliberately ignored.
2. **The key identifies a process instance, not a pid.** host_sessions
   is keyed by (claude-ancestor pid, boot id, process start time);
   start time kills pid-recycling aliasing within one boot. The table
   is a disposable cache: when its shape changes, `migrate` drops and
   recreates it instead of migrating (worst case a session re-seeds).
3. **The ancestor matcher tolerates wrapper renaming**: leading dots
   are stripped before the `claude` prefix check.

Rejected alternative — delivery-time reconciliation (hook drains a
sibling mailbox matched by workspace): with several parallel sessions
in one repo it would steal mail across genuinely different sessions,
breaking the one-mailbox-one-consumer invariant (ADR-0012) instead of
repairing identity.

## Consequences

- Hook and server converge on one mailbox regardless of compactions
  and server re-spawns; the silent-hook failure mode is structurally
  gone. Verified live: a hook carrying a rotated id drained the
  mailbox seeded under the server's spawn id.
- `claude --resume` in a fresh process still rejoins the old address:
  no map row exists yet, so the resumed id seeds it.
- The address a session gets is now "first id this claude process ever
  presented", which may differ from the *current* transcript id. The
  address was never meant to be parsed; whoami and the catalog remain
  the way to learn it.
- If the host ever rotates ids *and* re-spawns the serving connection
  atomically (so the old address truly dies), the map keeps the old
  address alive for the claude process — which is exactly the
  continuity we want: mail keeps flowing to one mailbox.
