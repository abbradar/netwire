.PHONY: all

package = $(or $(filter-out skeleton,$(patsubst %.cabal,%,$(wildcard *.cabal))),$(notdir $(realpath .)))

all: dist/setup-config $(package).cabal
	cabal-dev build
	cabal-dev test

$(package).cabal:
	sed -i -e 's/_PACKAGE_/$(package)/g' \
		.boring \
		LICENSE \
		README.md \
		Setup.lhs \
		program/Main.hs \
		skeleton.cabal \
		test/Bench.hs \
		test/Props.hs \
		test/Test.hs
	mv skeleton.cabal $(package).cabal
	darcs init
	darcs setpref boringfile .boring
	darcs add -r .
	darcs record -am "Initial revision."

dist/setup-config: $(package).cabal
	cabal-dev install -j4 --enable-tests --only-dependencies
	cabal-dev configure --enable-tests
