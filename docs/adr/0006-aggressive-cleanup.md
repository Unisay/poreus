# ADR-0006: Aggressive cleanup over backwards compatibility

## Status

Accepted — 2026-04-25.

## Context

The redesign (ADR-0001..ADR-0005) is breaking on multiple axes:

- Subcommands: `claim`, `complete`, `reject`, `status`,
  `watch-check`, `migrate` removed.
- Schema: tables `tasks`, `results`, `schema_version` dropped.
- Output format: `[POREUS:IN]/[POREUS:RESULT]` two-prefix
  scheme replaced by a single prefix.
- bash watch infrastructure (`poreus-watch-loop.sh`,
  `watch-alive.sh`) deleted.

A staged rollout (deprecate-then-remove over weeks) was
considered. It would minimize peer-agent disruption but cost:

- Months of dual-implementation maintenance.
- Confusion about which API to use for new code.
- A perpetual interval where consumers might have
  inconsistent expectations.

The system is single-host and single-user. All peer agents are
under the same admin and can be coordinated in real time.

## Decision

Cut once. The redesign ships in one revision: new CLI, new
schema, new bash-free watcher, new slash commands. No
deprecation aliases. A short coordinated outage (~10 minutes)
during cutover is accepted.

The cutover is sequenced:

1. claude-config prepares skill+command rewrites on a staging
   branch.
2. New binary deploys via nixos.
3. All bash watch loops are pkill'd; `$POREUS_HOME` is wiped.
4. claude-config merges staging.
5. Each peer session re-runs `poreus init && /poreus:register
   && /poreus:watch` on next visit.

## Consequences

**Positive**

- One canonical set of commands, schemas, and conventions
  immediately. No deprecation paperwork.
- Smaller code: no compatibility shims, dual code paths, or
  version branching in tests.

**Negative**

- All in-flight tasks at cutover time are lost (mitigated by
  ADR-0009 clean slate — losing data was already accepted).
- Active Claude sessions need manual re-init. There is no
  silent fallback — peers must notice and act.
- Anyone offline during the cutover misses the announcement
  and must figure out the upgrade from their next failed
  command.

## Alternatives Considered

- **Deprecation aliases for one release.** `poreus complete`
  → `poreus send` shim. Doable, but lifecycle is short
  (single-user system); the cost outweighs the benefit.
- **Side-by-side binaries (`poreus2`).** Forces every
  slash-command and config to choose. Lifecycle complexity
  larger than the cleanup it avoids.

## References

- Plan: `/home/yura/.claude-work/plans/robust-dazzling-flute.md`
- Related: ADR-0009 (clean slate).
