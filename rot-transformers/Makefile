all: init test_init
	cabal install

init:
	cabal sandbox init

test_init:
	cabal configure --enable-tests
	cabal install --enable-test --only-dependencies
	cabal build
	cabal test

docs:
	cabal haddock --hyperlink-source
