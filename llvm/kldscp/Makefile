CC 			:= clang
CXX 		:= clang++
LD 			:= clang++
LLVMCONF	:= llvm-config-3.1
CFLAGS 		:= -g $(shell $(LLVMCONF) --cflags)
CXXFLAGS	:= -g $(shell $(LLVMCONF) --cppflags)
LDFLAGS 	:= $(shell $(LLVMCONF) --libs core analysis codegen) $(shell $(LLVMCONF) --ldflags)

kld: main.o ast.o codegen.o
	$(LD) $^ $(LDFLAGS) -o $@

.c.o :
	$(CC) -c $< -o $@

.cpp.o :
	$(CXX) -std=c++11 $(CFLAGS) -c $< -o $@

.PHONY: clean, vim_clang
vim_clang:
	make CXX='~/.vim/bin/cc_args.py clang++'

clean:
	rm *.o
