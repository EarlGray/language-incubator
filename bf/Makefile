
SOURCE=bfhello.cpp

BF_SRC=brainfuck.cpp
MAIN_SRC=brhello.cpp


CC=gcc
CCFLAGS=-O3 -c -march=core2
LD=g++

default:
	$(CC) $(CCFLAGS) -o bf.o brainfuck.cpp
	ar rcs bf.a bf.o
	$(CC) $(CCFLAGS) -o bfhello.o bfhello.cpp 
	$(LD) -o bfi bfhello.o bf.a
all:	
	

clean:
	rm *.o
	rm *~
