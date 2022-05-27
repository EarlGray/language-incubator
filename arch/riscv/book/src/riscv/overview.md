# RISC-V basics

## The basic sets and extensions

The basic instruction sets are:
- **RV32I** for 32-bit integers and addresses;
- **RV64I** for 64-bit integers and addresses;
- Some thought was given to **RV128I** as well.

A specific implementation MUST implement the basic set (allowing for software
emulation of some instructions), and CAN implement _standard_ and _non-standard_
_extensions_.

Most common standard extensions:

- integer **M**ultiplication
- integer **A**tomic operations
- single-precision (32bit) **F**loating-point operations
- **D**ouble-precision (64bit) floating-point operations
- **C**ompressed instruction encoding

The **G**eneral variant, **RV*nn*G**, is a shortcut for **RV*nn*IMAFD**

A popular compilation target is the **GC** combination:

```console
$ rustup target list | grep riscv
riscv32i-unknown-none-elf
riscv32imac-unknown-none-elf
riscv32imc-unknown-none-elf
riscv64gc-unknown-linux-gnu
riscv64gc-unknown-none-elf 
riscv64imac-unknown-none-elf
```

## Endianness

The base set memory system is assumed to be _little-endian_ in respect to parcels.

16-bit parcels can be of any encoding, e.g.

```
    // Example: store x2 at x3 in native endianness
    sh   x2, 0(x3)    // store the low parcel of x2 at x3
    srli x2, x2, 16   // right-shift the integer by 16 bit
    sh   x2, 2(x3)    // store high parcel of x3
```

## Exceptions, traps, interrupts

_Exceptions_ are caused by not being to proceed with the normal execution of a
thread. _Trap_ is a synchronous transfer of control to a _trap handler_ (usually
executed in a more privileged environment).

_Interrupts_ are caused by events external to the current thread of execution.
