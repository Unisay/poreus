#!/usr/bin/env bash
# End-to-end MCP protocol smoke: pipe an initialize / tools-list /
# whoami conversation into `poreus serve` over a throwaway store and
# assert the responses with jq. Used by CI, the release workflow, and
# nix flake checks.
set -euo pipefail

BIN="${1:?usage: mcp-smoke.sh <poreus-binary>}"

export POREUS_HOME="$(mktemp -d)"
export POREUS_SESSION_ID="smoke-$$"
trap 'rm -rf "$POREUS_HOME"' EXIT

OUT=$(printf '%s\n' \
  '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"smoke","version":"0"}}}' \
  '{"jsonrpc":"2.0","method":"notifications/initialized"}' \
  '{"jsonrpc":"2.0","id":2,"method":"tools/list"}' \
  '{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"whoami","arguments":{}}}' \
  | "$BIN" serve)

echo "$OUT" | jq -e -s --arg addr "s-$POREUS_SESSION_ID" '
  length == 3
  and .[0].result.protocolVersion == "2025-06-18"
  and .[0].result.serverInfo.name == "poreus"
  and (.[0].result.instructions | length) > 0
  and (.[1].result.tools | length) == 12
  and .[2].result.structuredContent.address == $addr
  and (.[2].result.isError // false | not)
' > /dev/null

echo "MCP smoke OK: $($BIN version)"
