# ADR-0013: One binary, three entry modes

## Status

Accepted — 2026-08-14.

## Context

The reimplementation needs three process shapes: the long-lived MCP
server (one per session), a short-lived hook companion invoked by the
host's hook system, and occasional operator commands. They could ship
as separate binaries, or as one binary with argv dispatch.

The deployment path is a published contract: the `packages.poreus`
flake attribute and the nixos agent's `deploy-poreus` verb install a
single artifact. The hook and the server must also agree exactly on
identity derivation and delivery semantics — version skew between
them corrupts cursor bookkeeping.

## Decision

One binary, three entry modes:

- `poreus serve` — the MCP server (stdio JSON-RPC loop + 5 s tick).
- `poreus hook` — reads the host's hook JSON from stdin, delivers
  pending messages as context, always exits 0.
- `poreus admin purge [--older-than DAYS]`, `poreus version`.

Argv dispatch is a hand-rolled `getArgs` match;
`optparse-applicative` is dropped — four fixed argv shapes do not
warrant a parser library, and no human types these commands.

## Consequences

- The hook is version-locked to the server by construction: one
  store, one binary, one deploy.
- The published flake contract (`packages.poreus`, `deploy-poreus`)
  is untouched by the pivot.
- Shell completion, `--help` prose, and other CLI affordances are
  gone with the CLI itself (ADR-0010); `usage` on stderr is the only
  human-facing text.
