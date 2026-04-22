# poreus

Deterministic CLI for agent-to-agent (A2A) task delegation between Claude
Code sessions on the same machine. Replaces the legacy file-based
`a2a-queue/` store with a single SQLite database and a small set of
state-machine subcommands.

## Quick start

```bash
# Build the packed distribution binary:
nix build .#poreus
./result/bin/poreus --help

# Install to your user profile:
nix profile install .#poreus
```

## Dev shell

```bash
nix develop                  # GHC 9.6.6 + cabal + HLS + sqlite + upx + treefmt
cabal build all
cabal run poreus -- --help
```

## Using from another NixOS flake

Add `poreus` to your system flake as an input:

```nix
{
  inputs.poreus.url = "path:/home/yura/projects/poreus";
  # …
  environment.systemPackages = [ inputs.poreus.packages.${pkgs.system}.poreus ];
}
```

## Storage

State lives in `$POREUS_HOME/db.sqlite` (default
`$XDG_DATA_HOME/poreus`, fallback `~/.local/share/poreus`). Run
`poreus init` once to create it. `poreus migrate` imports the legacy
`~/.claude-work/a2a-queue/` tree.
