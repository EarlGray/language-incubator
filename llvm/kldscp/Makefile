CC := gcc
LD := gcc
CFLAGS := -g

kld: main.o ast.o
	$(LD) $^ -o $@

.c.o :
	$(CC) -g -c $< -o $@

.PHONY: clean
clean:
	rm *.o
