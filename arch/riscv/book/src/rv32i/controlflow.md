# Control Flow Instructions

| instr                             | description         |
|-----------------------------------|---------------------|
| **beq**  _rs1_, _rs2_, imm[12:1]  | Branch to `pc + sext(imm)` if _rs1_ = _rs2_
| **bne**  _rs1_, _rs2_, imm[12:1]  | Branch to `pc + sext(imm)` if _rs1_ ≠ _rs2_
| **blt**  _rs1_, _rs2_, imm[12:1]  | Branch to `pc + sext(imm)` if `(int)rs1 < (int)rs2_`
| **bltu** _rs1_, _rs2_, imm[12:1]  | Branch to `pc + sext(imm)` if `(uint)rs1 < (uint)rs2`
| **bge**  _rs1_, _rs2_, imm[12:1]  | Branch to `pc + sext(imm)` if `(int)rs1 >= (int)rs2_`
| **bgeu** _rs1_, _rs2_, imm[12:1]  | Branch to `pc + sext(imm)` if `(uint)rs1 >= (uint)rs2`
| **jal**  _rd_, imm[20:1]          | Jump and link
| **jalr** _rd_, _rs1_, imm[11:0]   | Jump and link register

## Conditional branches

A conditional jump to anywhere in range of ±4 KiB (1K instructions)
relative to `pc` (at 16 bit boundary).

## Jump-and-link

**jal** _rd_, _imm[20:1]_ (_jump-and-link_):

- writes the address of the subsequent instruction (`pc + 4`) to _rd_
- sets `pc` to `pc + sext(imm)`, allowing for jumps in a ±1MiB range.

Note: a one-way "goto" can be `jal x0, offset` to discard the "return" address.

"Long jumps" (to an arbitrary 32-bit `offset`) can be done with:
```
    auipc x1, offset[31:12]
    jalr  x0, offset[11:0](x1)
```

## Jump-and-link-register

**jalr** _rd_, _rs1_, _imm[11:0]_ allows for indirect jumps (switch statements,
function returns, indirect function calls, vtable dispatch, etc):

- write the address of the next instruction (`pc + 4`) into _rd_
- set `pc` to `pc + rs1 + sext(imm)`

TODO: if _rd_ is _rs1_, does it use the original value?


## Encoding

**jal** is [UJ-type](../riscv/encoding.md#uj-type-encoding).

|         |_imm[20,10:1,11,19:12]_|_rd_| _opcode_  |
|---------|-----------------------|----|-----------|
| **jal** |                       |    |`11 011 11`|

**jalr** is [I-type](../riscv/encoding.md#i-type-encoding).

|          | _imm[11:0]_ |_rs1_|_funct3_|_rd_| _opcode_  |
|----------|-------------|-----|--------|----|-----------|
| **jalr** |             |     | `000`  |    |`11 001 11`|


Conditional branches are [SB-type](../riscv/encoding.md#sb-type-encoding)

|          |_imm[12,10:5]_|_rs2_|_rs1_|_funct3_|_imm[4:1,11]_| _opcode_  |
|----------|--------------|-----|-----|--------|-------------|-----------|
| **beq**  |              |     |     | `000`  |             |`11 000 11`|
| **bne**  |              |     |     | `001`  |             |`11 000 11`|
| **blt**  |              |     |     | `100`  |             |`11 000 11`|
| **bge**  |              |     |     | `101`  |             |`11 000 11`|
| **bltu** |              |     |     | `110`  |             |`11 000 11`|
| **bgeu** |              |     |     | `111`  |             |`11 000 11`|


Least significant byte `6_`/`E_` encodes a branch instruction:

- `63`/`E3` for conditional jumps
- `67`/`E7` for **jalr**
- `6F`/`EF` for **jal**
