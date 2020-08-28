{
  pinnedPkgs ? import <nixpkgs> {}
, json
}:
let 
  nixpkgs = builtins.fromJSON (builtins.readFile json);
  src = pinnedPkgs.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs-channels";
    inherit (nixpkgs) rev sha256;
  };
in 
  import src {}