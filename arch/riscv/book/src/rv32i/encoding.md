# Instruction Encoding

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
  ..xxxx | xxxxxxxx xxxxxxxx | xNNNxxxx x1111111 |     (80 + 16*NNN)-bit, NNN ≠ 111)

```

## Encodings

Every RV32I instruction is 32 bit long.

It is encoded by one of the six encoding types ([**R-type**](#r-type-encoding),
[**I-type**](#i-type-encoding), [**S-type**](#s-type-encoding)/[**SB-type**](#sb-type-encoding),
[**U-type**](#u-type-encoding)/[**UJ-type**](#uj-type-encoding))
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


### R-type encoding

Contains _rd_, _funct3_, _rs1_, _rs2_, _funct7_

```
 bit      R-type           
_____       ___            
  31         |             
  30         |             
  29         |             
  28         | funct7      
  27         |             
  26         |             
  25        _|_            
__24         |             
  23         |             
  22         | rs2         
  21         |             
  20        _|_            
  19         |             
  18         |             
  17         | rs1         
__16         |             
  15        _|_            
  14         |             
  13         | funct3      
  12        _|_            
  11         |             
  10         |             
   9         | rd          
___8         |             
   7        _|_            
   6         |             
   5         |             
   4         |             
   3         | opcode      
   2         |             
   1         | 1           
___0        _|_1           

```

### I-type encoding

Contains _rd_, _funct3_, _rs1_, 12-bit _imm_

```
 bit      I-type           
_____       ___            
  31         |             
  30         |             
  29         |             
  28         |             
  27         |             
  26         | imm[11:0]   
  25         |             
__24         |             
  23         |             
  22         |             
  21         |             
  20        _|_            
  19         |             
  18         |             
  17         | rs1         
__16         |             
  15        _|_            
  14         |             
  13         |  funct3     
  12        _|_            
  11         |             
  10         |             
   9         |  rd         
___8         |             
   7        _|_            
   6         |             
   5         |             
   4         |             
   3         | opcode      
   2         |             
   1         |             
___0        _|_            

```


### S-type encoding

Contains _funct3_, _rs1_, _rs2_, 12-bit _imm_ (_imm[11:5]_ at bit 25,
_imm[4:0]_ at bit 7).

```
 bit      S-type         
_____       ___          
  31         |           
  30         |           
  29         |           
  28         | imm[11:5] 
  27         |           
  26         |           
  25        _|_          
__24         |           
  23         |           
  22         |  rs2      
  21         |           
  20        _|_          
  19         |           
  18         |           
  17         |  rs1      
__16         |           
  15        _|_          
  14         |           
  13         |  funct3   
  12        _|_          
  11         |           
  10         |           
   9         | imm[4:0]  
___8         |           
   7        _|_          
   6         |           
   5         |           
   4         |           
   3         |  opcode   
   2         |           
   1         |           
___0        _|_          

```


### SB-type encoding

Like S-type, but without imm[0], imm[12] at bit 31, imm[11] at bit 7

```
 bit      SB-type         
_____       ___          
  31         |_imm[12]          
  30         | imm[10]   
  29         | imm[9]
  28         | imm[8]
  27         | imm[7]       
  26         | imm[6]       
  25        _|_imm[5]    
__24         |           
  23         |           
  22         |  rs2      
  21         |           
  20        _|_          
  19         |           
  18         |           
  17         |  rs1      
__16         |           
  15        _|_          
  14         |           
  13         |  funct3   
  12        _|_          
  11         | imm[4]          
  10         | imm[3]
   9         | imm[2]
___8         |_imm[1]
   7        _|_imm[11]
   6         |           
   5         |           
   4         |           
   3         |  opcode   
   2         |           
   1         |           
___0        _|_          

```

### U-type encoding

Contains _rd_, 20-bit _imm_ at [31:12]

```
 bit      U-type         
_____       ___          
  31         |           
  30         |           
  29         |           
  28         |           
  27         |           
  26         |           
  25         |           
__24         |           
  23         |           
  22         |           
  21         | imm[31:12]
  20         |           
  19         |           
  18         |           
  17         |           
__16         |           
  15         |           
  14         |           
  13         |           
  12        _|_          
  11         |           
  10         |           
   9         |  rd       
___8         |           
   7        _|_          
   6         |           
   5         |           
   4         |           
   3         |  opcode   
   2         |           
   1         |           
___0        _|_          

```


### UJ-type encoding

Like U-type, but _imm_ without imm[0], imm[20] at 31, imm[10:1] at 21, imm[11] at 20, imm[19:12] at 12

```
 bit      U-type         
_____       ___          
  31         |_imm[20]
  30         | imm[10]   
  29         | imm[9]   
  28         | imm[8]       
  27         | imm[7]       
  26         | imm[6]       
  25         | imm[5]       
__24         | imm[4]       
  23         | imm[3]       
  22         | imm[2]       
  21         |_imm[1]
  20         |_imm[11]     
  19         | imm[19]          
  18         | imm[18]          
  17         | imm[17]          
__16         | imm[16]          
  15         | imm[15]          
  14         | imm[14]          
  13         | imm[13]          
  12        _|_imm[12]          
  11         |           
  10         |           
   9         |  rd       
___8         |           
   7        _|_          
   6         |           
   5         |           
   4         |           
   3         |  opcode   
   2         |           
   1         |           
___0        _|_          

```

