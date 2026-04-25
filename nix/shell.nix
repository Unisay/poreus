{ pkgs, project }:

# Flat `pkgs.mkShell { packages = … }` instead of `project.shellFor`
# + `inputsFrom`: under nixpkgs 25.05+ `mkShell.mergeInputs` tries to
# `evalModules` each element of `inputsFrom`, and the haskell.nix shell
# derivation carries env attrs (LANG, LC_ALL, …) that the module system
# rejects. Building tools individually via `project.tool` sidesteps that
# entirely — each tool is a plain derivation, suitable for `packages`.

let
  ghc = pkgs.haskell-nix.compiler.ghc966;
  cabal = project.tool "cabal" "latest";
  hls = project.tool "haskell-language-server" "latest";
  fourmolu = project.tool "fourmolu" "latest";
  hlint = project.tool "hlint" "latest";
  cabal-fmt = project.tool "cabal-fmt" "latest";
in
pkgs.mkShell {
  name = "poreus-shell";

  packages = [
    ghc
    cabal
    hls
    fourmolu
    hlint
    cabal-fmt
    pkgs.sqlite-interactive
    pkgs.upx
    pkgs.cachix
    pkgs.treefmt
    pkgs.nixpkgs-fmt
    pkgs.git
    pkgs.which
    pkgs.file
    pkgs.jq
    pkgs.zlib
    pkgs.pkg-config
  ];

  shellHook = ''
    export PS1="\n\[\033[1;32m\][poreus:\w]\$\[\033[0m\] "
  '';
}
