{ compiler ? "ghc884", pkgs ? (import ./nix/pinned.nix {} {}).pkgs }:
let
  caani = pkgs.haskell.packages.${compiler}.callCabal2nix "caani" ./. {};
in 
  caani
