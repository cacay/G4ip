all: build


build:
	cabal build

test:
	cabal test

clean:
	cabal clean
