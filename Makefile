.PHONY: all

package = netwire

all: dist/setup-config $(package).cabal
	cabal-dev build

dist/setup-config: $(package).cabal
	cabal-dev install -j4 --enable-tests --only-dependencies
	cabal-dev configure --enable-tests
