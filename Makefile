.PHONY: all deploy init nix

package = netwire


all: dist/setup-config $(package).cabal
	cabal-dev build

deploy:
	cabal-dev sdist
	nix-env -f . -iA netwire

init:
	darcs init
	darcs setpref boringfile .boring
	darcs add -r .

nix:
	cabal-dev sdist
	nix-build -A netwire


dist/setup-config: $(package).cabal
	cabal-dev install -j4 -ftestprogram --enable-tests --only-dependencies
	cabal-dev configure -ftestprogram --enable-tests
