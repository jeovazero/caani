{ compiler ? "ghc865" }:
let
  pkgs = import ./nix/source.nix { json = ./nix/source.json; };
  caani =
    if pkgs.stdenv.isLinux
    then
      import ./static.nix {}
    else
      pkgs.haskell.lib.justStaticExecutables (import ./release.nix {});
  tar = pkgs.gnutar;
  upx = pkgs.upx;
in 
pkgs.stdenv.mkDerivation {
    name = "artifact";
    buildInputs = [ tar upx ];
    src = ./resources;
    buildPhase = "mkdir -p $out/caani/resources";
    installPhase = ''
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
