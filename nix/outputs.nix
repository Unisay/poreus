{ inputs, system }:

let
  pkgs = import ./pkgs.nix { inherit inputs system; };
  inherit (pkgs) lib;

  project = import ./project.nix { inherit inputs pkgs lib; };

  # Dynamically linked executable — fast iteration.
  poreusDynamic = project.hsPkgs.poreus.components.exes.poreus;

  # Test-suite component (dynamic linking).
  poreusTests = project.hsPkgs.poreus.components.tests.poreus-test;

  # Statically linked via haskell.nix's musl cross.
  poreusStatic =
    if system == "x86_64-linux"
    then project.projectCross.musl64.hsPkgs.poreus.components.exes.poreus
    else poreusDynamic;

  # Distribution target: static + upx.
  poreusPackaged =
    if system == "x86_64-linux"
    then import ./poreus-upx.nix { inherit pkgs; staticPoreus = poreusStatic; }
    else poreusDynamic;

  shell = import ./shell.nix { inherit pkgs project; };

  packages = {
    default = poreusDynamic;
    poreus-dynamic = poreusDynamic;
    poreus-static = poreusStatic;
    poreus = poreusPackaged;
    poreus-tests = poreusTests;
  };

  apps = {
    default = {
      type = "app";
      program = "${poreusPackaged}/bin/poreus";
    };
    poreus = {
      type = "app";
      program = "${poreusPackaged}/bin/poreus";
    };
  };

  devShells = {
    default = shell;
  };

  checks = {
    poreus-build = poreusDynamic;
    poreus-tests = poreusTests;
  };

in
{
  inherit packages apps devShells checks;
}
