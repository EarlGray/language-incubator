
colorizer:  colorizer.o  lexer.o
	gcc -o colorizer colorizer.o lexer.o

colorizer.o:	colorizer.c
	gcc -c colorizer.c -o colorizer.o

lexer.o:	lexer.yy.c
	gcc -c lexer.yy.c -o lexer.o

lexer.yy.c:	lexer.lex
	flex -o lexer.yy.c lexer.lex

clean:  
	rm *.o *.yy.c
