# Single source of truth for the package version: poreus.cabal.
# Everything in nix/ that needs pname/version reads this instead of
# hardcoding (the old poreus-upx.nix carried a stale 0.1.0.0).
let
  cabal = builtins.readFile ../poreus.cabal;
  match = builtins.match ".*[\n]version:[[:space:]]*([0-9.]+)[\n].*" cabal;
in
assert match != null;
{
  pname = "poreus";
  version = builtins.head match;
}
