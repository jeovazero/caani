let
  compiler = "ghc884";
  staticNixRepo = builtins.fetchGit {
    "url" = "git@github.com:nh2/static-haskell-nix.git";
    "rev" = "382150290ba43b6eb41981c1ab3b32aa31798140";
  };
  pinned = import ./nix/pinned.nix { withPatches = true; };
  myPkgs = (pinned { system = "x86_64-linux"; }).pkgs;
  staticPkgs = (import "${staticNixRepo}/survey/default.nix" {
    inherit compiler;
    pkgs = myPkgs.pkgsMusl;
    defaultCabalPackageVersionComingWithGhc = "Cabal_3_2_0_0";
  }).pkgs;
  caani = import ./release.nix { pkgs = staticPkgs; inherit compiler; };
  static = drv:
    staticPkgs.haskell.lib.overrideCabal
    drv
    (old: {
      doCheck = false;
      enableSharedExecutables = false;
      enableSharedLibraries = false;
      configureFlags = [
        "--ghc-option=-optl=-static"
        "--extra-lib-dirs=${staticPkgs.ncurses.override { enableStatic = true; }}/lib"
        "--extra-lib-dirs=${staticPkgs.gmp6.override { withStatic = true; }}/lib"
        "--extra-lib-dirs=${staticPkgs.zlib.static}/lib"
        "--extra-lib-dirs=${staticPkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
        "--disable-executable-stripping"
        "--disable-executable-dynamic"
        "--disable-library-profiling"
      ];
    });
in 
  static caani
