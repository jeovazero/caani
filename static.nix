{ compiler ? "ghc865" }:
let
  pkgs = (import ./nix/source.nix { json = ./nix/source.json; });
  caani = import ./release.nix {};
  static = drv:
      pkgs.pkgsMusl.haskell.lib.overrideCabal
            drv
            (old: {
                  doCheck = false;
                  enableSharedExecutables = false;
                  enableSharedLibraries = false;
                  configureFlags = [
                    "--ghc-option=-optl=-static"
                    "--extra-lib-dirs=${pkgs.pkgsMusl.ncurses.override { enableStatic = true; }}/lib"
                    "--extra-lib-dirs=${pkgs.pkgsMusl.gmp6.override { withStatic = true; }}/lib"
                    "--extra-lib-dirs=${pkgs.pkgsMusl.zlib.static}/lib"
                    "--extra-lib-dirs=${pkgs.pkgsMusl.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
                    "--extra-lib-dirs=${pkgs.pkgsMusl.glibc.static}/lib"
                    "--disable-executable-stripping"
                    "--disable-executable-dynamic"
                    "--disable-library-profiling"
                  ];
            });
in 
  static caani
