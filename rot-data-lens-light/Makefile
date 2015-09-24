all: init sandbox_stuff test_init
	cabal install

init:
	cabal sandbox init

sandbox_stuff:
	cabal sandbox add-source ../rot-transformers

test_init:
	cabal install --enable-test --only-dependencies
	cabal configure --enable-tests
	cabal build
	cabal test

docs:
	cabal haddock --hyperlink-source
