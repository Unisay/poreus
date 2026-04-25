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

  # Cross-compiled aarch64-linux static binary (only available when the
  # build host is x86_64-linux; haskell.nix drives the cross via musl).
  poreusStaticAarch64Linux =
    project.projectCross.aarch64-multiplatform-musl.hsPkgs.poreus.components.exes.poreus;

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
  } // lib.optionalAttrs (system == "x86_64-linux") {
    poreus-static-aarch64-linux = poreusStaticAarch64Linux;
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
