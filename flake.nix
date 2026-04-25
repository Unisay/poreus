{
  description = "poreus — deterministic CLI for agent-to-agent task delegation";

  inputs = {
    haskell-nix = {
      url = "github:input-output-hk/haskell.nix/7d6abbfef8cc2a385bbd94906a6cf66f61747a3f";
      inputs.hackage.follows = "hackage";
    };

    nixpkgs.follows = "haskell-nix/nixpkgs";

    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    import ./nix/outputs.nix { inherit inputs system; }
  );

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://poreus.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "poreus.cachix.org-1:ULsv/3tUwR50uuRMedlv2eM7rarsBy3r7FW1vArJ3nA="
    ];
    allow-import-from-derivation = true;
    accept-flake-config = true;
  };
}
