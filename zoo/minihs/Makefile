CMOFILES    := syntax.cmo type_check.cmo interpret.cmo parser.cmo lexer.cmo minihs.cmo

OCAMLC      = ocamlc
OCAMLLEX    = ocamllex
OCAMLYACC   = ocamlyacc

.PHONY: clean

minihs: $(CMOFILES)
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
