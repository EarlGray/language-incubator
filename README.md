language-incubator
==================

An incubator/sandbox for learning compilers, interpreters, code generation, virtual machines, assemblers, JITs, etc.

Hacks that have grown into separate projects:

- [x86/hasm](https://github.com/EarlGray/hasm): a simple x86 assembler written in Haskell, for now it's opcode set is very limited;
- [fp/secd](https://github.com/EarlGray/SECD): implementation of SECD virtual machine in C with a Scheme-to-SECD compiler;

Some protoprojects that may be interesting:

- js/slothjs: an effort at a naïve, primitive, savage Javascript interpreter (with parsing offloaded to [Esprima](https://esprima.org/));
- bf/bfjc: a primitive but fully functional x86\_64 JIT compiler for BrainFuck, inspired by [_Adventures in JIT compilation_ by eliben](https://eli.thegreenplace.net/2017/adventures-in-jit-compilation-part-1-an-interpreter/) ;
- fp/tapl: reimplementation of some typecheckers from [TAPL](https://www.cis.upenn.edu/~bcpierce/tapl/) (in Haskell/Prolog), e.g. [fullsimple](https://github.com/EarlGray/language-snippets/blob/master/fp/tapl/fullsimple.pl).
- llvm/kldscp: LLVM tutorial, JIT compiler of a toy language Kaleidoscope;
- forth/mips/mforth.S: rewrite of the awesome [jonesforth](https://github.com/AlexandreAbreu/jonesforth) from x86/Linux to MIPS/Linux (tested on CI20);
- c/longjmp/x86: a hackish reimplementation of `setjmp`/`longjmp` compliant to GCC x86 ABI;
- zoo/{calc, miniml, minihs, miniprolog, boa}: a mirror of the awesome [PLZoo](http://andrej.com/plzoo/) ;
- rel/chr: small Constraint Handling Rules examples;
