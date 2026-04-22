{ inputs, pkgs, lib }:

pkgs.haskell-nix.cabalProject' (
  { config, pkgs, ... }:
  {
    name = "poreus";

    compiler-nix-name = lib.mkDefault "ghc966";

    src = lib.cleanSource ../.;

    flake.variants = { ghc966 = { }; };

    modules = [
      {
        packages.poreus.components.exes.poreus = {
          # Produce a self-contained executable. For static builds, this is
          # wrapped by projectCross.musl64 below; the dynamic variant keeps
          # default linking for quick iteration.
          dontStrip = false;
        };
      }
    ];
  }
)
