SECD machine
============

This is a loose implementation of [SECD machine](http://en.wikipedia.org/wiki/SECD). 
It's state is representated as a tuple of 4 lists:
* S for (computational) stack
* E for environment:
        Environment is a list of frames.
        Each frame is a pair (cons) of two lists:
            the first for symbol names, 
            the second for bound values.
        The first frame represents the global environment and build-in routines.
* C for control flow (list of opcodes to execute)
* D for dump (stack): it's for storing S/E/C that must be restored later

Also there is `scm2secd.scm` which is a compiler from Scheme to SECD codes written in Scheme. It is not self-hosted yet, you need some existing interpreter to be installed, I use `mzscheme` from Dr.Racket.

Memory is managed using reference counting at the moment, a simple optional garbage collection is on my TODO-list. 

Opcodes and operational semantics
---------------------------------

Boolean values are now `#t` and NIL.
Only C int types are supported as integers
The current opcode set and the operational semantics is:

    ADD, SUB, MUL, DIV, REM
            :     ((x y. s), e, c, d) -> ((x _op_ y . s), e, c, d)
    LEQ     :     ((x y. s), e, c, d) -> ((x < y? #t : nil).s, e, c, d)

    CAR     :     ((x._) . s), e, c, d)  -> (x.s, e, c, d)
    CDR     :     ((_.x) . s), e, c, d)  -> (x.s, e, c, d)
    CONS    :     ((x y . s), e, c, d)   -> ((x.y).s, e, c, d)
       
    LDC v   :     (s, e, c, d)           -> (v.s, e, c, d)
    LD sym  :     (s, e, c, d)           -> ((*lookup* e sym).s, e, c, d)`

    ATOM    :     (v.s, e, c, d)         -> ((*atom?* v).s, e, c, d)` 
    EQ      :     (v1 v2 . s, e, c, d)   -> ((*eq* v1 v2).s, e, c, d)`

    SEL tb eb:    (v.s, e, c, d)         -> (s, e, (if v then thenb else elseb), (c.d))`
    JOIN    :     (s, e, nil, s0.e0.c0.d) -> (s.s0, e0, c0, d)`

    LDF     :     (s, e, (args body).c, d) -> ((args body).e, e, c, d)
    AP      :     (((argnames c1) . e1).argvals.s, e, c, d) -> 
                                           (nil, (zip argnames argvals).e1, c1, s.e.c.d)`
    RTN    :      (v, e, nil, s0.e0.c0.d) -> (v.s0, e0, c0, d)
    DUM    :      (s, e, c, d)            -> (s, dummy.e, c, d)
    RAP    :      ((c'.e').v'.s, e, c, d) -> (nil, frame(v').e', c', (s, # todo

    PRINT`:    side-effect of printing the head of S
    READ`:     puts the input s-expression on top of S


How to run
----------

Examples of running the SECD codes:

```bash
$ gcc secd.c -o secd
$ echo "(LDC 2  LDC 2  ADD PRINT)" | ./secd
4

$ ./secd < tests/test_fact.secd
720

# read from file first, then from the stdin
$ cat tests/test_io.secd - | ./secd
1
(Looks like an atom)
^D
```

See `tests/` directory for more examples of closures/recursion/IO in SECD codes.

Examples of compiling Scheme files into SECD codes:

    # a working scheme interpreter is required, `mzscheme` here:
    $ mzscheme -f scm2secd.scm <tests/append.scm 2>/dev/null >append.secd


Examples of running Scheme files using a bootstrapping interpreter:

    $ cat tests/append.scm | mzscheme -f scm2secd.scm 2>/dev/null | ./secd
    (1 2 3 4 5 6)

