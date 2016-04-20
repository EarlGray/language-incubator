EXE := hello
SRC := hello.pl
MAIN := hello

.PHONY: exec clean

$(EXE): $(SRC)
	swipl -o $@ -g hello -c $<

exec:
	@swipl -f $(SRC) -g $(MAIN) -q

clean:
	rm -f $(EXE)
