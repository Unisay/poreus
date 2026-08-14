# ADR-0010: Reimplement poreus as an MCP server; retire the CLI

## Status

Accepted — 2026-08-14.

## Context

poreus v0.2 was a CLI held by LLM agents through a skill document and
seven slash commands. Telemetry across 1409 sessions showed a stable
class of misuses (retired verbs, malformed flags, shell-quoting
mangling of rich text) that documentation kept defending against but
never eliminated. Three structural causes:

1. **Text plumbing.** `send` needed three input modes (stdin JSON,
   flag-mode, `--summary-file`/`--payload-file`) purely because shell
   quoting mangles apostrophes, Cyrillic, and EM-DASH.
2. **Documentation-as-defense.** One generic `send` verb forced agents
   to reconstruct conventions (reply duty, kind/event shapes) from a
   cheat-sheet held in context.
3. **No session identity.** Short-lived CLI processes forced the fcntl
   lock + pidfile + `$CLAUDE_CODE_SSE_PORT` machinery, and delivery
   silently stopped whenever nobody remembered to start `inbox -f`.

MCP removes each cause: typed tool arguments (no shell), schemas and
tool descriptions carried in-band (no external document), and a
server process that is session-scoped by construction.

## Decision

Rewrite poreus as an MCP server (`poreus serve`), spawned per session
by the host over stdio. The v0.2 CLI surface is retired entirely: no
subcommands survive except the operator's `admin purge` and
`version`. The functional contract is
`docs/design/functional-spec-mcp.md`; the wire contract is
`docs/design/protocol.md` v2.

Purpose-built tools replace the generic `send`: `request`, `call`,
`reply`, `notify` — the wrong shape is unexpressible rather than
documented-against. The `[POREUS:IN]` line format, the exit-code API,
and the anti-misuse hint scaffolding all disappear.

## Consequences

- The consumer layer shrinks to policy only (autonomy handling,
  adoption etiquette); the protocol duty travels in the server's
  `initialize.instructions`.
- Non-MCP consumers (scripts, cron) lose direct access; if
  scriptability proves needed, a thin non-interactive client over the
  same store is additive later (spec OQ-9, consciously deferred).
- The clean-slate cutover follows ADR-0006/0009: no data migration,
  snapshot first, wipe, every session restarts.
