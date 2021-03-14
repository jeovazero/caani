let
  compiler = "ghc884";
  staticNixRepo = builtins.fetchTarball {
    name = "static-haskell-nix";
    url = "https://github.com/nh2/static-haskell-nix/archive/382150290ba43b6eb41981c1ab3b32aa31798140.tar.gz";
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "0zsyplzf1k235rl26irm27y5ljd8ciayw80q575msxa69a9y2nvd";
  };
  pinned = import ./nix/pinned.nix { withPatches = true; };
  myPkgs = (pinned { system = "x86_64-linux"; }).pkgs;
  staticPkgs = (import "${staticNixRepo}/survey/default.nix" {
    inherit compiler;
    pkgs = myPkgs.pkgsMusl;
    integer-simple = true;
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
