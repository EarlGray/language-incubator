# RV32I: Computational Instructions

There are 21 computational instructions.

_rd_ denotes a _d_estination _r_egister, _rs1_ and _rs2_ some _s_ource _r_egisters.
_imm_ stands for "immediate value".


| Instruction                 | "C"                                       | Meaning              |
|-----------------------------|-------------------------------------------|----------------------|
| **add** _rd_, _rs1_, _rs2_  | _rd_ = _rs1_ + _rs2_                      |
| **sub** _rd_, _rs1_, _rs2_  | _rd_ = _rs1_ - _rs2_                      |
| **sll** _rd_, _rs1_, _rs2_  | _rd_ = _rs1_ << _rs2_                     | shift left logical by register
| **srl** _rd_, _rs1_, _rs2_  | TODO                                      | shift right logical by register
| **sra** _rd_, _rs1_, _rs2_  | TODO                                      | shift right arithm. by register
| **and** _rd_, _rs1_, _rs2_  | _rd_ = _rs1_ \& _rs2_                     | bitwise AND
| **or**  _rd_, _rs1_, _rs2_  | _rd_ = _rs1_ \| _rs2_                     | bitwise OR
| **xor** _rd_, _rs1_, _rs2_  | _rd_ = _rs1_ ^ _rs2_                      | bitwise XOR
| **slt** _rd_, _rs1_, _rs2_  | _rd_ = ((int)_rs1_ < (int)_rs2_)          | compare, set 1/0
| **sltu** _rd_, _rs1_, _rs2_ | _rd_ = ((uint)_rs1_ < (uint)_rs2_)        | compare, set 1/0
| **addi** _rd_, _rs1_, _imm_ | _rd_ = _rs1_ + (int32\_t)imm              |
| **slli** _rd_, _rs1_, _imm_ | _rd_ = _rs1_ << imm[4:0]                  |
| **srli** _rd_, _rs1_, _imm_ | ???                                       | shift right logical by immediate
| **srai** _rd_, _rs1_, _imm_ | ???                                       | shift right arithm. by immediate
| **andi** _rd_, _rs1_, _imm_ | _rd_ = _rs1_ \& imm                       | bitwise AND with immediate
| **ori**  _rd_, _rs1_, _imm_ | _rd_ = _rs1_ \| imm                       | bitwise OR with immediate
| **xori** _rd_, _rs1_, _imm_ | _rd_ = _rs1_ ^ imm                        | bitwise XOR with immediate
| **slti** _rd_, _rs1_, _imm_ | _rd_ = ((int)_rs1_ < (int)imm)            | sign-extend imm, compare, set 1/0
| **sltui** _rd_, _rs1_, imm_ | _rd_ = ((uint)_rs1_ < (uint)imm)          | sign-extend imm, compare as unsigned, set 1/0
| **lui** _rd_, _imm_         | _rd_ = (imm << 12)                        | load upper immediate, set lower 12 bits to 0
| **auipc** _rd_, _imm_       | _rd_ = pc + (imm << 12)                   | add upper immediate to `pc`


Signed integers are stored as 2's complements. All of instructions sign-extend operands if needed.


Definitions:
- **logical left shift by _n_**: equivalent to multiplication by 2^n.
- **logical right shift by _n_**: equivalent to unsigned division by 2^n, rounding towards 0
- **logical arithmetical shift by _n_**: equivalent to unsigned division by 2^n, rounding down

Notes:

- **not** _rd_, _rs1_ can be implemented as `xori rd, rs1, -1`
- a pseudoinstruction **seqz**: `sltiu rd, rs1, 1`, computes if _rs1_ is 0.
- a pseudoinstruction **li** _rd_, _imm_: `lui rd, imm[31:12]; addi rd, rd, imm[11:0]`
- **nop** is usually defined as `addi x0, x0, 0`

TODO: what happens on integer overflow?

`auipc` is a position-independent code shortcut, e.g.:

```
auipc   x4, 0x1
lw      x4, 0x234(x4)
```

allows to read a word from memory at `pc + 0x1234` into `x4`

TODO: `pc` at which point?


## Encoding `auipc` and `lui`

`lui` and `auipc` are [U-type](../riscv/encoding.md#u-type-encoding).
Least significant byte looks like 37/B7, 17/97.

|           |imm[31:12]|_rd_| opcode      |
|-----------|----------|----|-------------|
| **lui**   |          |    | `01 101 11` |
| **auipc** |          |    | `00 101 11` |


## Encoding register instructions

Instructions with _rs2_ are [R-type](../riscv/encoding.md#r-type-encoding):

|          |_funct7_ |_rs2_|_rs1_|_funct3_|_rd_ | opcode    |
|----------|---------|-----|-----|--------|-----|-----------|
| **add**  |`0000000`|     |     | `000`  |     |`01 100 11`|
| **sub**  |`0100000`|     |     | `000`  |     |`01 100 11`|
| **sll**  |`0000000`|     |     | `001`  |     |`01 100 11`|
| **slt**  |`0000000`|     |     | `010`  |     |`01 100 11`|
| **sltu** |`0000000`|     |     | `011`  |     |`01 100 11`|
| **xor**  |`0000000`|     |     | `100`  |     |`01 100 11`|
| **srl**  |`0000000`|     |     | `101`  |     |`01 100 11`|
| **sra**  |`0100000`|     |     | `101`  |     |`01 100 11`|
| **or**   |`0000000`|     |     | `110`  |     |`01 100 11`|
| **and**  |`0000000`|     |     | `111`  |     |`01 100 11`|


## Encoding instructions with immediates

Everything else is [I-type](../riscv/encoding.md#i-type-encoding).

|          |imm[11:5]|imm[4:0]|_rs1_|_funct3_|_rd_ | opcode    |
|----------|---------|--------|-----|--------|-----|-----------|
| **addi** |         |        |     | `000`  |     |`00 100 11`|
| **slti** |         |        |     | `010`  |     |`00 100 11`|
| **sltiu**|         |        |     | `011`  |     |`00 100 11`|
| **xori** |         |        |     | `100`  |     |`00 100 11`|
| **ori**  |         |        |     | `110`  |     |`00 100 11`|
| **andi** |         |        |     | `111`  |     |`00 100 11`|
| **slli** |`0000000`|  shamt |     | `001`  |     |`00 100 11`|
| **srli** |`0000000`|  shamt |     | `101`  |     |`00 100 11`|
| **srai** |`0100000`|  shamt |     | `101`  |     |`00 100 11`|

## Least significant byte

- `13`/`93` for instructions with immediates
- `33`/`B3` for register instructions
- `37`/`B7` for **lui**
- `17`/`97` for **auipc**
