all: build

dev:
	ghcid --command "ghci src/Main -fobject-code -i.:src" --test main

build:
	nix-build release.nix

artifact:
	nix-build artifact.nix

format:
	stylish-haskell -irv .

lint:
	hlint .
