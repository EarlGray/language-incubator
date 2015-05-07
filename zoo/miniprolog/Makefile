CMOFILES    := syntax.cmo unify.cmo solve.cmo parser.cmo lexer.cmo message.cmo miniprolog.cmo

OCAMLC      = ocamlc
OCAMLLEX    = ocamllex
OCAMLYACC   = ocamlyacc

.PHONY: clean

miniprolog: $(CMOFILES)
	$(OCAMLC) -o $@ $^

%.cmo: %.ml
	$(OCAMLC) -c $<

%.ml: %.mli
	$(OCAMLC) -c $<

%.ml: %.mll
	$(OCAMLLEX) $<

%.mli: %.mly
	$(OCAMLYACC) $<

clean:
	rm parser.ml parser.mli lexer.ml ; rm *.cmo *.cmi ; true

run: miniprolog
	$$(which rlwrap) ./miniprolog
