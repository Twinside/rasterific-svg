
all:
	stack build

prof:
	stack build --profile --ghc-options="-rtsopts"

lint:
	hlint .

doc:
	cabal haddock

