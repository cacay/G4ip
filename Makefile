all: build


build:
	ghc -o g4ip -O --make Main


clean:
	rm *.o *.hi g4ip