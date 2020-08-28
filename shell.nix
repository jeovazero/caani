{ nixpkgs ? import ./nix/source.nix { json = ./nix/source.json; }
 }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskell;
  haskellPackages = haskell.packages.ghc865;
  cabal2nix = pkgs.cabal2nix;

  project = (import ./release.nix {});
in
pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = project.env.nativeBuildInputs ++ [
    haskellPackages.cabal-install
    pkgs.fira-code
    haskellPackages.ghcid
    cabal2nix
  ];
  # https://github.com/NixOS/nix/issues/599
  LOCALE_ARCHIVE="/usr/lib/locale/locale-archive";

  # https://github.com/mpickering/hie-bios/issues/25#issuecomment-537718071
  shellHook='' 
    export  NIX_GHC="$(which ghc)"
    export  NIX_GHCPKG="$(which ghc-pkg)"
    export  NIX_GHC_DOCDIR="$NIX_GHC/../../share/doc/ghc/html"
    export  NIX_GHC_LIBDIR="$(ghc --print-libdir)"
  '';
}
