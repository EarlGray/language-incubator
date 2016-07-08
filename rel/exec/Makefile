EXE := hello
SRC := hello.pl
MAIN := hello

.PHONY: exec clean

$(EXE): $(SRC)
	swipl -o $@ -g $(MAIN) -c $<

$(EXE).standalone: $(SRC)
	swipl -o $@ -g $(MAIN) -c $< --stand_alone=true

exec:
	@swipl -f $(SRC) -g $(MAIN) -q

clean:
	rm -f $(EXE)
