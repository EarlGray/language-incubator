SECD machine
============

This is a loose implementation of [SECD machine][1]. 
It's state is representated as a tuple of 4 lists:
  S for (computational) stack
  E for environment:
        Environment is a list of frames.
        Each frame is a pair (cons) of two lists:
            the first for symbol names, 
            the second for bound values.
        The first frame represents the global environment and build-in routines.
  C for control flow (list of opcodes to execute)
  D for dump (stack): it's for storing S/E/C that must be restored later

The current opcode set is:

    `ADD`, `SUB`, `MUL`, `DIV`, `REM`:
                `((s1 s2 . s), e, c, d) -> ((s1 _op_ s2 . s), e, c, d)`

    `CAR` :     `((x._) . s), e, c, d)  -> (x.s, e, c, d)`
    `CDR` :     `((_.x) . s), e, c, d)  -> (x.s, e, c, d)`
    `CONS`:     `((x y . s), e, c, d)   -> ((x.y).s, e, c, d)`
       
    `LDC v`:    `(s, e, c, d)           -> (v.s, e, c, d)`
    `LD sym`:   `(s, e, c, d)           -> ((*lookup* e sym).s, e, c, d)`

    `ATOM`:     `(v.s, e, c, d)         -> ((*atom?* v).s, e, c, d)` 
    `EQ`:       `(v1 v2 . s, e, c, d)   -> ((*eq* v1 v2).s, e, c, d)`

    `SEL thenb elseb`: `(v.s, e, c, d)  -> (s, e, (if v then thenb else elseb), (c.d))`

    `AP`:

[1] http://en.wikipedia.org/wiki/SECD
