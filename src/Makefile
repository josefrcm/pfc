HS = ghc
SRC = src
HSFLAGS = -XPatternGuards -fglasgow-exts -Wall -funbox-strict-fields

.PHONY: all clean


# all: $(REGLAS)
all:
	@cd $(SRC) && $(HS) $(HSFLAGS) --make -O -o migracion Main.hs
	@mv src/migracion .
	@strip migracion

clean:
	@-rm -f $(SRC)/*.hi $(SRC)/*/*.hi
	@-rm -f $(SRC)/*.o $(SRC)/*/*.o
