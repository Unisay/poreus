# ADR-0011: Hand-rolled newline-delimited JSON-RPC framing

## Status

Accepted — 2026-08-14.

## Context

MCP's stdio transport frames messages as newline-delimited JSON-RPC
2.0 — one message per line, UTF-8, no Content-Length headers. The
Haskell MCP library ecosystem is immature, drags in heavy dependency
trees (a musl static cross-build constraint — Template Haskell and
plugin-style code paths are the usual cross-compilation blockers),
and would still leave the interesting parts (tool registry, error
taxonomy, delivery semantics) to us.

## Decision

Implement the framing and JSON-RPC layer by hand:

- `Poreus.Mcp.Framing` — a two-function `Transport` record
  (recv line / send frame), stdio-backed in production, injected in
  tests so the protocol loop runs as pure values.
- `Poreus.Mcp.JsonRpc` — request/notification parsing and
  result/error/notification builders over plain aeson `Value`s.
  Request ids are opaque and echoed verbatim.
- **No batching.** JSON-RPC batch arrays are not accepted; MCP hosts
  do not send them over stdio.
- Tool schemas are hand-built aeson values — no TH, no generics
  derivation for the wire (musl cross-build stays clean).

## Consequences

- The whole protocol layer is ~300 lines and fully covered by
  deterministic protocol-loop tests (`[Value] -> TestIOM [Value]`).
- Protocol-version negotiation is ours to maintain: the server
  supports 2024-11-05 / 2025-03-26 / 2025-06-18 and echoes a
  supported client version, else offers its newest.
- If MCP moves the stdio framing (e.g. to header-delimited), the
  change is confined to `Poreus.Mcp.Framing`.
