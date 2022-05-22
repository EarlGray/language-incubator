# Introduction to RISC-V

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
- **C**compressed instruction encoding

The **G**eneral variant, **RVxxG**, is a shortcut for **RVxxIMAFD**


## Instruction Length Encoding

The base set contains fixed-length 32-bit instructions, naturally aligned to 
32-bit boundaries. This instructions have the five least significant bits set to
`xxx11` except `11111` (see below).

The encoding supports variable-length instructions of one or more 16bit
_parcels_ (with least significant bits 00, 01, 10).

The standard **C** (compressed) extension relaxes boundaries to be 16bit.

```
                              ___________________ 
                             | xxxxxxxx xxxxxxAA |     16-bit (AA ≠ 11)

          _______________________________________
         | xxxxxxxx xxxxxxxx | xxxxxxxx xxxBBB11 |     32-bit (BBB ≠ 111)

  _______________________________________________
  ..xxxx | xxxxxxxx xxxxxxxx | xxxxxxxx xx011111 |     48-bit

  _______________________________________________
  ..xxxx | xxxxxxxx xxxxxxxx | xxxxxxxx x0111111 |     64-bit

  _______________________________________________
  ..xxxx | xxxxxxxx xxxxxxxx | xNNNxxxx x1111111 |     (80 + 16*NNN)-bit, nnn ≠ 111)

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

