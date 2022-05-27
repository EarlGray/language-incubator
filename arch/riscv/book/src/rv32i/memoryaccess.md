# RV32I: Memory Access Instructions

[Computational](computational.md) instructions only work on the contents of
registers. Memory access instruction exchange 8/16/32-bit values ("bytes"/
"halfs"/"words") between registers and RAM locations.

There are 5 _load_ ("RAM to register") instructions and 3 _store_
("register to RAM") instructions. They using byte addresses in RAM encoded
as the value in register _rs1_ added with 12-bit sign-extended immediate.

Misaligned RAM access is allowed, but can be non-atomic and/or much slower.

## Load instructions

All in [I-type format](../riscv/encoding.md#i-type-encoding).

| instr   | description              | "C" |
|---------|--------------------------|-----|
| **lw**  | "load word"              | `rd = *(int32_t *)(rs1 + imm[11:0])`
| **lh**  | "load half", sign-extend | `rd = (int32_t)(*(int16_t *)(rs1 + imm[11:0]))`
| **lb**  | "load byte", sign-extend | `rd = (int32_t)(*(int8_t *)(rs1 + imm[11:0]))`, sign-extend
| **lhu** | "load half unsigned"     | `rd = (uint32_t)(*(uint16_t *)(rs1 + imm[11:0]))`, zero-extend
| **lbu** | "load byte unsigned"     | `rd = (uint32_t)(*(uint8_t *)(rs1 + imm[11:0]))`, zero-extend


## Store instructions

All in [S-type format](../riscv/encoding.md#s-type-encoding).

| instr   | description  | "C" |
|---------|--------------|-----|
| **sw**  | "store word" | `*(int32_t *)(rs1 + imm[11:0])) = rs2`
| **sh**  | "store half" | `*(int16_t *)(rs1 + imm[11:0])) = (int16_t)rs2`
| **sb**  | "store byte" | `*(int8_t *)(rs1 + imm[11:0])) = (int8_t)rs2`


## Encoding

| instr |_imm[11:0]_|_rs1_|_funct3_|_rd_| _opcode_    |
|-------|-----------|-----|--------|----|-------------|
|**lb** |           |     | `000`  |    | `00 000 11` |
|**lh** |           |     | `001`  |    | `00 000 11` |
|**lw** |           |     | `010`  |    | `00 000 11` |
|**lbu**|           |     | `100`  |    | `00 000 11` |
|**lhu**|           |     | `101`  |    | `00 000 11` |

<br>

| instr |_imm[11:5]_|_rs2_|_rs1_|_funct3_|_imm[4:0]_| _opcode_    |
|-------|-----------|-----|-----|--------|----------|-------------|
|**sb** |           |     |     | `000`  |          | `01 000 11` |
|**sh** |           |     |     | `001`  |          | `01 000 11` |
|**sw** |           |     |     | `010`  |          | `01 000 11` |

