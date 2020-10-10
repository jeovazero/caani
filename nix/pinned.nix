{ withPatches ? false }:
let
  pinnedPkgs = builtins.fetchTarball {
    name = "nixos-20.09-beta-2020-10-06";
    url = "https://github.com/nixos/nixpkgs/archive/7badbf18c45b7490d893452beb8950d966327831.tar.gz";
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "0snk9dvj4hxg1ch704symxfz2kic06l0vf1b5aax9gdlflzg8fmj";
  };
  hostPkgs = import pinnedPkgs {};

  patches = [
    # https://github.com/NixOS/nixpkgs/issues/85924#issuecomment-640277067
    ./patches/fix-broken-ghc-865-binary.patch
  ];
 
  patchedPkgs = hostPkgs.runCommand "nixpkgs-20.09-beta-2020-10-06-patched"
    {
     inherit pinnedPkgs;
     inherit patches;
    }
    ''
      cp -r $pinnedPkgs $out
      chmod -R +w $out
      for p in $patches; do
        echo "Applying patch $p";
        patch -d $out -p1 < "$p";
      done
    '';
 in import (if withPatches then patchedPkgs else pinnedPkgs)