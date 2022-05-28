# RV32I

## RV32I registers 

The user-visible architectural state is:

- Value of `x0`/`zero` is always 0; writes do not change it.
- 31 read/write data registers `x1` .. `x31`;
- `pc`: the _program counter_ register pointing to the currently executing
  instruction.

Instructions must be stored naturally aligned in little-endian byte order.


## Instruction classes

RV32I specification contains 47 instructions:

- [computational instructions](rv32i/computational.md) (21 instructions)
- [memory access instructions](rv32i/memoryaccess.md) (10 instructions)
- [control flow instructions](rv32i/controlflow.md) (8 instructions)
- [system instructions](rv32i/system.md) (8 instructions, control and status registers)


## Major opcodes for classes

op[1:0]=**11** for all instructions in RV32I.

|       op[4:2]= | 000     |    001     |    010   |   011      |   100     |   101       |   110     |
|----------------|---------|------------|----------|------------|-----------|-------------|-----------|
| op[6:5]=**00** |  Loads  |  _F-ext_   |          |  Fences    | Arithm    |**`AUIPC`**  |  _RV64I_  |
| op[6:5]=**01** |  Stores |  _F-ext_   |          |  _A-ext_   | Arithm    |**`LUI`**    |  _RV64I_  |
| op[6:5]=**10** |  _F-ext_|  _F-ext_   |  _F-ext_ |  _F-ext_   | _F-ext_   |             |  _RV128I_ |
| op[6:5]=**11** | Branches| **`JALR`** |          |  **`JAL`** | System    |             |  _RV128I_ |
         


## Least significant byte of an instruction by class

| Byte          |  _3         |   _7        |    _B    |    _F    |
|---------------|-------------|-------------|----------|----------|
| **0_**/**8_** | Loads       | _F-ext_     |          | Fences   |
| **1_**/**9_** | Arithm. (I) | **`AUIPC`** | _RV64I_  |          |
| **2_**/**A_** | Stores      | _F-ext_     |          | _A-ext_  |
| **3_**/**B_** | Arithm. (R) | **`LUI`**   | _RV64I_  |          |
| **4_**/**C_** | _F-ext_     | _F-ext_     | _F-ext_  |          |
| **5_**/**D_** | _F-ext_     |             | _RV128I_ |          |
| **6_**/**E_** | Branches    | **`JALR`**  |          |**`JAL`** |
| **7_**/**F_** | System      |             | _RV128I_ |          |

