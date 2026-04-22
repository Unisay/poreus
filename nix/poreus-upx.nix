{ pkgs, staticPoreus }:

# Take the statically linked `poreus` binary, strip it hard, and repack with
# `upx --best --lzma`. Result: a single-file distribution-ready binary under
# $out/bin/poreus. Verifies via `file` that the input is genuinely static
# before packing (upx on a dynamically linked binary would be wrong).

pkgs.stdenvNoCC.mkDerivation {
  pname = "poreus";
  version = "0.1.0.0";

  dontUnpack = true;

  nativeBuildInputs = [
    pkgs.upx
    pkgs.file
    pkgs.binutils
  ];

  buildPhase = ''
    runHook preBuild

    cp ${staticPoreus}/bin/poreus ./poreus
    chmod +w ./poreus

    echo "--- checking input binary type ---"
    file ./poreus
    if file ./poreus | grep -q "dynamically linked"; then
      echo "ERROR: input binary is dynamically linked; refusing to UPX-pack." >&2
      exit 1
    fi

    echo "--- stripping symbols ---"
    strip --strip-unneeded ./poreus || true

    echo "--- running upx --best --lzma ---"
    upx --best --lzma ./poreus

    echo "--- final binary ---"
    file ./poreus
    ls -l ./poreus

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    install -Dm755 ./poreus $out/bin/poreus
    runHook postInstall
  '';

  meta = {
    description = "poreus — deterministic CLI for A2A task delegation (static + UPX)";
    mainProgram = "poreus";
    platforms = pkgs.lib.platforms.linux;
  };
}
