{ pinned ? import ./nix/pinned.nix {} } :
let
  nixpkgs = pinned {};
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskell cabal-install cabal2nix stylish-haskell hlint;
  haskellPackages = haskell.packages.ghc884;
  project = (import ./release.nix {});
in
pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = project.env.nativeBuildInputs ++ [
    cabal-install
    haskellPackages.ghcid
    cabal2nix
    stylish-haskell
    hlint
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
