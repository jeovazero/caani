{ compiler ? "ghc865" }:
let
  pkgs = import ./nix/source.nix { json = ./nix/source.json; };
  caani = pkgs.haskell.packages.${compiler}.callCabal2nix "caani" ./. {};
in 
  pkgs.haskell.lib.justStaticExecutables caani
