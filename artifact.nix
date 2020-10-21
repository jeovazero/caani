{ compiler ? "ghc884", pinned ? import ./nix/pinned.nix {}, isDynamic ? false }:
let
  nixpkgs = pinned {};
  inherit (nixpkgs) pkgs;
  inherit (pkgs) stdenv;
  caani-release = import ./release.nix {};
  caani =
    if stdenv.isLinux && !isDynamic
    then import ./static.nix
    else pkgs.haskell.lib.justStaticExecutables caani-release;
  tar = pkgs.gnutar;
  upx = pkgs.upx;
in 
stdenv.mkDerivation {
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
