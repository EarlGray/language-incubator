
LEXF=lisp.lex
YF=lisp.yy.c
OUTF=lisplex

RESRC=../../lisp/dict.lisp

run:	$(OUTF)
	./$(OUTF) < $(RESRC)

$(OUTF):    $(YF)
	gcc -o $(OUTF) $(YF)

$(YF):	$(LEXF)
	flex -o $(YF)	$(LEXF)
    
