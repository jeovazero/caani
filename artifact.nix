{ compiler ? "ghc884", pkgs ? import ./nix/pinned.nix }:
let
  caani = pkgs.haskell.lib.justStaticExecutables (import ./release.nix {});
  tar = pkgs.gnutar;
  upx = pkgs.upx;
in 
pkgs.stdenv.mkDerivation {
    name = "artifact";
    buildInputs = [ tar upx ];
    src = ./resources;
    installPhase = ''
      mkdir -p $out/caani/resources
      cp . $out/caani/resources -rv
      cp ${caani}/bin/caani $out/caani
      cd $out/caani
      chmod 755 caani
      upx caani
      chmod 555 caani
      cd $out
      tar -czf caani.tar.gz caani/
    '';
}
