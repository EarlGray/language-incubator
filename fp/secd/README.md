SECD machine
============

This is a loose implementation of [SECD machine](http://en.wikipedia.org/wiki/SECD) and a simplest Scheme-to-SECD compiler, which is self-hosted.

The design is mostly inspired by detailed description in _Functional programming: Application and Implementation_ by Peter Henderson and his LispKit.

Opcodes and operational semantics
---------------------------------

The machine's state is represented as tuple of 4 lists:
* S for (computational) stack
* E for environment:
        Environment is a list of frames.
        Each frame is a pair (cons) of two lists:
            the first for symbol names,
            the second for bound values.
        The first frame represents the global environment and build-in routines.
* C for control flow (list of opcodes to execute)
* D for dump (stack): it's for storing S/E/C that must be restored later

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
    LD sym  :     (s, e, c, d)           -> ((*lookup* e sym).s, e, c, d)

    ATOM    :     (v.s, e, c, d)         -> ((*atom?* v).s, e, c, d)
    EQ      :     (v1 v2 . s, e, c, d)   -> ((*eq* v1 v2).s, e, c, d)

    SEL tb eb:    (v.s, e, c, d)         -> (s, e, (if v then thenb else elseb), (c.d))
    JOIN    :     (s, e, nil, s0.e0.c0.d) -> (s.s0, e0, c0, d)`

    LDF     :     (s, e, (args body).c, d) -> ((args body).e, e, c, d)
    AP      :     (((argnames c1) . e1).argvals.s, e, c, d) ->
                                           (nil, (zip argnames argvals).e1, c1, s.e.c.d)
    RTN     :      (v, e, nil, s0.e0.c0.d) -> (v.s0, e0, c0, d)
    DUM     :      (s, e, c, d)            -> (s, dummy.e, c, d)
    RAP     :      ((c'.e').v'.s, e, c, d) -> (nil, frame(v').e', c', (s, # todo

    PRINT   :     side-effect of printing the head of S
    READ    :     puts the input s-expression on top of S

Memory is managed using reference counting at the moment, a simple optional garbage collection is on my TODO-list.

Values are persistant, immutable and shared.

`READ`/`PRINT` are implemented as built-in commands in C code.
There are some functions which are implemented in C for efficiency:
- `append`: is heavily used by the compiler;
- `list`: see above;
- `null?`, `number?`, `symbol?`: may be implemented in native code only;

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


Scheme compiler: scm2secd.scm
-----------------------------

This file a simplest compiler from Scheme to SECD code, written in Scheme. It is written in a quite limited subset of Scheme (even without `define`, using `let`/`letrec` instead). Also there's *no* support for tail-recursion optimization yet, that's why it is not strictly a Scheme in the meaning of RnRS. Also, it supports very limited set of types (`symbol`s, `number`s and `list`s: no vectors, bytestrings, chars, strings, etc).

The compiler is self-hosted and can be bootstrapped using its compiled SECD code in `scm2secd.secd`:

```bash
# self-bootstrapping:
$ ./secd scm2secd.secd <scm2secd.scm >scm2secd.secd

# or, using a bootstrapping interpreter:
$ guile -s scm2secd.scm <scm2secd.scm >scm2secd.secd
$ mzscheme -f scm2secd.scm <scm2secd.scm >scm2secd.secd
```

Due to limitations of SECD code described by P.Henderson, `eval` can't be implemented now (TODO: add command `LDFS`, LoaD Function from Stack).
Use such way of evaluating Scheme:
```bash
$ ./secd scm2secd.secd <tests/append.scm | ./secd
(1 2 3 4 5 6)
```

TODO
----
- `LDFS`: LoaD Function from Stack, to implement `eval`;
- `AP n`: treat n values on top of the stack as arguments for a function call instead of always creating a list.
- Is there a way to make `cond` forms less nested?
- support for more Scheme types: `bytestring`, `string` (list of chars?), `port`;
- make symbol storage: quick access (balanced binary search tree or hashtable?), reuse string resources;
- tail-call optimization.
- optional garbage-collector, compare RefCount and GC speed.
- LLVM backend?


