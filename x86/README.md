HASM
====

HASM is an x86 32-bit assembler which tries to mimic GAS. 

For now, it supports only few commands and requires input in terms of Haskell types:

    **HASM**> OpPush (OpndReg RegEAX)
    50

    **HASM**> OpPush (OpndImmL 0x804841c)
    68 1c 84 04 08

    **HASM**> OpAdd (OpndImmL 0x1d42) (OpndReg RegEBX)
    81 c3 42 1d 00 00
