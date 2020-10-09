import (builtins.fetchTarball {
  name = "nixos-20.09-beta-2020-10-06";
  url = "https://github.com/nixos/nixpkgs/archive/7badbf18c45b7490d893452beb8950d966327831.tar.gz";
  # Hash obtained using `nix-prefetch-url --unpack <url>`
  sha256 = "0snk9dvj4hxg1ch704symxfz2kic06l0vf1b5aax9gdlflzg8fmj";
}) {}
