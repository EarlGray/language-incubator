CC 			:= gcc
CXX 		:= g++
LD 			:= g++
LLVMCONF	:= llvm-config-3.1
CFLAGS 		:= -g $(shell $(LLVMCONF) --cflags) -Wall -Werror
CXXFLAGS	:= -g $(shell $(LLVMCONF) --cppflags)
LDFLAGS 	:= $(shell $(LLVMCONF) --libs core native jit) $(shell $(LLVMCONF) --ldflags)

kld: ast.o codegen.o main.o
	$(LD) $^ $(LDFLAGS) -o $@

.c.o :
	$(CC) $(CFLAGS) -c $< -o $@

.cpp.o :
	$(CXX) -std=gnu++0x $(CXXFLAGS) -c $< -o $@

.PHONY: clean, vim_clang
vim_clang:
	make CXX='~/.vim/bin/cc_args.py clang++'

clean:
	rm *.o
