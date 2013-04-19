CC := gcc
LD := gcc
CFLAGS := -g

kld: main.o lexer.o
	$(LD) $^ -o $@

.c.o :
	$(CC) -g -c $< -o $@

#main.o: main.c
#lexer.o: lexer.c

.PHONY: clean
clean:
	rm *.o
