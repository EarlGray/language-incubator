CMOFILES    := syntax.cmo type_check.cmo machine.cmo compile.cmo parser.cmo lexer.cmo miniml.cmo

OCAMLC      = ocamlc
OCAMLLEX    = ocamllex
OCAMLYACC   = ocamlyacc

.PHONY: clean

miniml: $(CMOFILES)
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
	rm parser.ml parser.mli lexer.ml ; rm *.cmo *.cmi
