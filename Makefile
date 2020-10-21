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

ci:
	nix-shell --run "sh ci/run.sh"

cp-bin:
	cp $$(find dist-newstyle -name caani -type f) ./caani