{ compiler ? "ghc865" }:
let
  pkgs = import ./nix/source.nix { json = ./nix/source.json; };
  caani = import ./release.nix {};
  tar = pkgs.gnutar;
in 
pkgs.stdenv.mkDerivation {
    name = "artifact";
    buildInputs = [ tar ];
    src = ./resources;
    buildPhase = "mkdir -p $out/caani/resources";
    installPhase = ''
      cp . $out/caani/resources -rv
      cp ${caani}/bin/caani $out/caani
      cd $out
      tar -czf caani.tar.gz caani/
    '';
}
