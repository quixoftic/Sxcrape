CABAL=~/Library/Haskell/bin/cabal-dev

all: local

local:
	$(CABAL) install --prefix=$(HOME) --user

sdist:
	$(CABAL) sdist

check:
	$(CABAL) check

build:
	$(CABAL) build

clean:
	$(CABAL) clean
