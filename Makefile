all: build

dev:
	ghcid --command "ghci src/Main -fobject-code -i.:src" --test main

build:
	nix-build release.nix

artifact:
	nix-build artifact.nix

cabal2nix:
	cabal2nix . > default.nix

init-cabal-nix:
	nix-shell -p cabal2nix --run 'cabal2nix . > default.nix'
