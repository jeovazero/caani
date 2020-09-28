{ nixpkgs ? import ./nix/source.nix { json = ./nix/source.json; }
 }:
let
  inherit (nixpkgs) pkgs;

  project =  pkgs.haskell.lib.justStaticExecutables (import ./release.nix {});
  resources = import ./resources.nix {};
in

pkgs.dockerTools.buildImage {
  name = "jeovazero/caani";
  contents = [ resources ];
  created = "now";
  config = {
    Cmd = [ "${project}/bin/caani" ];
    Env = [ "RESOURCE_DIR=/data" "PATH=$PATH:${project}/bin" ];
  };
}
