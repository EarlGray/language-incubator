# RV32I

## RV32I registers 

The user-visible architectural state is:

- Value of `x0`/`zero` is always 0, it's a read-only register.
- 31 registers `x1` .. `x31`;
- the `pc` register

Instructions must be stored naturally aligned in little-endian byte order.


## Encodings

Every RV32I instruction is 32 bit long. It is encoded by one of the six encoding types (R, U, S/SB, U/UJ)
and may contain the following parts:

| part     |instruction bits| description               | which encoding types   |
|----------|----------------|---------------------------|------------------------|
| _opcode_ | 7 at [6:0]     | operation selector        | (in all encoding types)
| _funct3_ | 3 at [14:12]   | suboperation selector     | (except U/UJ type)
| _funct7_ | 7 at [31:25]   | suboperation selector     | (only in R type)
| _rd_     | 5 at [11:7]    | Destination Register index| (except S/SB type)
| _rs1_    | 5 at [19:15]   | Source 1 Register index   | (except U/UJ type)
| _rs2_    | 5 at [24:20]   | Source 2 Register index   | (R, S/SB type)
| _imm_    | 5/7/12/20 bits | an immediate value        | (except R-type)

RV32I has six encoding patterns:

- **R-type**: _rd_, _funct3_, _rs1_, _rs2_, _funct7_
- **I-type**: _rd_, _funct3_, _rs1_, 12-bit _imm_
- **S-type**: _funct3_, _rs1_, _rs2_, 12-bit _imm_ at [31:20]
- **SB-type**: like S-type, but without imm[0], imm[12] at bit 31, imm[11] at bit 7
- **U-type**: _rd_, 20-bit _imm_ at [31:12]
- **UJ-type**: like U-type, but _imm_ without imm[0], imm[20] at 31, imm[10:1] at 21, imm[11] at 20, imm[19:12] at 12


```
 bit      R-type               I-type               S-type              U-type         
_____       ___                  ___                  ___                 ___          
  31         |                    |                    |                   |           
  30         |                    |                    |                   |           
  29         |                    |                    |                   |           
  28         | funct7             |                    | imm[11:5]         |           
  27         |                    |                    |                   |           
  26         |                    | imm[11:0]          |                   |           
  25        _|_                   |                   _|_                  |           
__24         |                    |                    |                   |           
  23         |                    |                    |                   |           
  22         | rs2                |                    |  rs2              |           
  21         |                    |                    |                   | imm[31:12]
  20        _|_                  _|_                  _|_                  |           
  19         |                    |                    |                   |           
  18         |                    |                    |                   |           
  17         | rs1                | rs1                |  rs1              |           
__16         |                    |                    |                   |           
  15        _|_                  _|_                  _|_                  |           
  14         |                    |                    |                   |           
  13         | funct3             |  funct3            |  funct3           |           
  12        _|_                  _|_                  _|_                 _|_          
  11         |                    |                    |                   |           
  10         |                    |                    |                   |           
   9         | rd                 |  rd                | imm[4:0]          |  rd       
___8         |                    |                    |                   |           
   7        _|_                  _|_                  _|_                 _|_          
   6         |                    |                    |                   |           
   5         |                    |                    |                   |           
   4         |                    |                    |                   |           
   3         | opcode             | opcode             |  opcode           |  opcode   
   2         |                    |                    |                   |           
   1         | 1                  |                    |                   |           
___0        _|_1                 _|_                  _|_                 _|_          

```


## Major opcodes

op[1:0]=**11** for all instructions in RV32I.

|       op[4:2]= | 000     |    001     |    010   |   011      |   100     |   101       |   110     |
|----------------|---------|------------|----------|------------|-----------|-------------|-----------|
| op[6:5]=**00** |  Loads  |  _F-ext_   |          |  Fences    | Arithm    |**`AUIPC`**  |  _RV64I_  |
| op[6:5]=**01** |  Stores |  _F-ext_   |          |  _A-ext_   | Arithm    |**`LUI`**    |  _RV64I_  |
| op[6:5]=**10** |  _F-ext_|  _F-ext_   |  _F-ext_ |  _F-ext_   | _F-ext_   |             |  _RV128I_ |
| op[6:5]=**11** | Branches| **`JALR`** |          |  **`JAL`** | System    |             |  _RV128I_ |
         


## Least significant byte of an instruction looks like...

| Byte          |  _3     |   _7        |    _B    |    _F    |
|---------------|---------|-------------|----------|----------|
| **0_**/**8_** | Loads   | _F-ext_     |          | Fences   |
| **1_**/**9_** | Arithm. | **`AUIPC`** | _RV64I_  |          |
| **2_**/**A_** | Stores  | _F-ext_     |          | _A-ext_  |
| **3_**/**B_** | Arithm. | **`LUI`**   | _RV64I_  |          |
| **4_**/**C_** | _F-ext_ | _F-ext_     | _F-ext_  |          |
| **5_**/**D_** | _F-ext_ |             | _RV128I_ |          |
| **6_**/**E_** | Branches| **`JALR`**  |          |**`JAL`** |
| **7_**/**F_** | System  |             | _RV128I_ |          |


## Instruction classes

- [computational instructions](rv32i/computational.md)
- memory access instructions
- memory access ordering
- control flow instructions
- system instructions
