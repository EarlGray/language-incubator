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

| instr                     | description              | "C" |
|---------------------------|--------------------------|-----|
| **lw**  _rd_, _imm_(_rs1_)| "load word"              | `int32_t *ptr = rs1 + (int32_t)imm;`<br>`rd = *ptr;`
| **lh**  _rd_, _imm_(_rs1_)| "load half",<br> sign-extend | `int16_t *p = rs1 + (int32_t)imm;`<br>`rd = (int32_t)*p;`
| **lb**  _rd_, _imm_(_rs1_)| "load byte",<br> sign-extend | `int8_t *p = rs1 + (int32_t)imm;`<br>`rd = (int32_t)*p;`
| **lhu** _rd_, _imm_(_rs1_)| "load half unsigned"     | `uint16_t *p = rs1 + (int32_t)imm;`<br>`rd = (uint32_t)*p;`
| **lbu** _rd_, _imm_(_rs1_)| "load byte unsigned"     | `uint8_t *p = rs1 + (int32_t)imm;`<br>`rd = (uint32_t)*p;`


## Store instructions

All in [S-type format](../riscv/encoding.md#s-type-encoding).

| instr                      | description  | "C" |
|----------------------------|--------------|-----|
| **sw** _imm_(_rs1_), _rs2_ | "store word" | `*(int32_t *)(rs1 + imm[11:0])) = rs2`
| **sh** _imm_(_rs1_), _rs2_ | "store half" | `*(int16_t *)(rs1 + imm[11:0])) = (int16_t)rs2`
| **sb** _imm_(_rs1_), _rs2_ | "store byte" | `*(int8_t *)(rs1 + imm[11:0])) = (int8_t)rs2`


## Fence instructions

| instr                    | description              |
|--------------------------|--------------------------|
| **fence** _pred_, _succ_ | an explicit barrier for the specified kinds of <br>concurrent memory accesses
| **fence.i**              | an explicit barrier for writing and executing <br>instructions in RAM concurrently

When multiple _harts_, hardware threads ("cores") are present and share
the same RAM, it is necessary to control how changes by one hart are perceived
by another.

Some (ahem, x86\_64) architectures provide _sequential consistency_, which
guarantees that any observed state can be described by some combination of
concurrent sequential changes. This model makes it easier to reason about
machine code, but can significantly complicate hardware. Under sequential
consistency, [speculative](https://en.wikipedia.org/wiki/Speculative_execution)
and [out-of-order](https://en.wikipedia.org/wiki/Out-of-order_execution)
execution must maintain a separate externally visible sequentially-consistent
state.

Since different harts work with different areas of RAM most of the time,
RISC-V assumes a _relaxed memory model_, which requires explicit synchronization
when needed.

A **fence** instruction provides an ordering guarantee between memory accesses
before and after the fence. The arguments describe:

1. the _predecessor set_: kinds of accesses by prior instructions that must be
   completed before **fence**
2. the _successor set_: kinds of accesses by subsequent instructions that must
   not start before the **fence**

The kinds of accesses are:

- R: "read memory"
- W: "write memory"
- I: "device input"
- O: "device output"

E.g. `fence rw, w` guarantees that all reads and writes by preceding
instructions appear completed before this instruction and any reordered writes
by subseqent instructions must wait until this instruction.
Note: reads by subsequent instructions can happen before this fence.

A **fence.i** allows to synchronize RAM data-access and instruction-access. E.g.
if one hart writes instructions to RAM and another executes them, **fence.i**
guarantees that preceding stores by one hart become visible to instruction
fetches from another hart after.


## Encoding

Stores are in [S-type format](../riscv/encoding.md#s-type-format):

| instr |_imm[11:5]_|_rs2_|_rs1_|_funct3_|_imm[4:0]_| _opcode_    |
|-------|-----------|-----|-----|--------|----------|-------------|
|**sb** |           |     |     | `000`  |          | `01 000 11` |
|**sh** |           |     |     | `001`  |          | `01 000 11` |
|**sw** |           |     |     | `010`  |          | `01 000 11` |


The following instructions are in [I-type format](../riscv/encoding.md#i-type-encoding):

| instr |_imm[11:0]_|_rs1_|_funct3_|_rd_| _opcode_    |
|-------|-----------|-----|--------|----|-------------|
|**lb** |           |     | `000`  |    | `00 000 11` |
|**lh** |           |     | `001`  |    | `00 000 11` |
|**lw** |           |     | `010`  |    | `00 000 11` |
|**lbu**|           |     | `100`  |    | `00 000 11` |
|**lhu**|           |     | `101`  |    | `00 000 11` |

<br>


| instr     | _imm[11:0]_           | _rs1_ |_funct3_|_rd_   | _opcode_    |
|-----------|-----------------------|-------|--------|-------|-------------|
|**fence**  |`0000`  _pred_ _succ_  |`00000`| `000`  |`00000`| `00 011 11` |
|**fence.i**|`0000 0000 00000`      |`00000`| `001`  |`00000`| `00 011 11` |

Least significant byte looks like:

- `03`/`83` for loads
- `23`/`A3` for stores
- `0F` for fences

TODO: clarify encoding of _pred_/_succ_ masks.
