# System Instructions

## OS interaction

There are two instructions to interact with the operating system:

- **ecall** for system calls
- **ebreak** for calling a debugger

## Control and Status Registers (CSRs)

Control and Status Registers (CSRs) provide a general facility
for system control and I/O. There is a CSR address space for up to 2<sup>12</sup>
registers.

The instructions to modify CSRs are:

| instr using _rs1_            | instr using _imm_             | description   |
|------------------------------|-------------------------------|---------------|
| **csrrw** _rd_, _csr_, _rs1_ | **csrrwi** _rd_, _csr_, _imm_ | atomically copy a value from _csr_ to _rd_ and <br>overwrite _csr_ with the value in _rs1_ or _imm_
| **csrrc** _rd_, _csr_, _rs1_ | **csrrci** _rd_, _csr_, _imm_ | atomically copy a value from _csr_ to _rd_ and <br>clear bits in _csr_ [^mask]
| **csrrs** _rd_, _csr_, _rs1_ | **csrrsi** _rd_, _csr_, _imm_ | atomically copy a value from _csr_ to _rd_ and <br>set bits in a _csr_ [^mask]

Note: `csrrs x1, csr, x0` can be used to read from `csr` without modifying it. 
It is abbreviated as a pseudoinstruciton **csrr** _rd_, _csr_.


## Mandatory user-readable CSRs

| CSR        | description                       |
|------------|-----------------------------------|
| `cycle`    | cycle counter                     |
| `cycleh`   | upper 32 bit of cycle counter     |
| `time`     | real-time clock                   |
| `timeh`    | upper 32 bit of real-time clock   |
| `instret`  | instructions retired counter      |
| `instreth` | upper 32 bit of instret           |


## Encoding

All of the following are in [I-format](../riscv/encoding.md#i-type-encoding):

|              | _imm[11:0]_    | _rs1_ |_funct3_| _rd_  | `opcode`  |
|--------------|----------------|-------|--------|-------|-----------|
| **ecall**    |`0000 0000 0000`|`00000`| `000`  |`00000`|`11 100 11`|
| **ebreak**   |`0000 0000 0001`|`00000`| `000`  |`00000`|`11 100 11`|
| **csrrw**    |                |       |  ???   |       |`11 100 11`|
| **csrrc**    |                |       |  ???   |       |`11 100 11`|
| **csrrs**    |                |       |  ???   |       |`11 100 11`|
| **csrrwi**   |                |       |  ???   |       |`11 100 11`|
| **csrrci**   |                |       |  ???   |       |`11 100 11`|
| **csrrsi**   |                |       |  ???   |       |`11 100 11`|


---

[^mask] TODO: according to the mask in _rs1_/_imm_ ?
