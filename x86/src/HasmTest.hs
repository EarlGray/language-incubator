module Main where

import HasmTypes
import HasmParse
import HasmCodegen
import HasmPretty

{-
 -  Test Examples
 -}
movinstr = HasmStInstr [] . Operation OpMov 
intinstr = HasmStInstr [] . Operation OpInt
linux_null_s = [
  HasmStLabel "_start",                           -- _start:
  movinstr [OpndImm (ImmL 0x0), OpndReg (RegL RegEAX)],  --    movl $1, %eax
  movinstr [OpndImm (ImmL 0x1), OpndReg (RegL RegEBX)],  --    movl $0, %ebx
  intinstr [OpndImm (ImmB 0x80)]               ]  --    int 0x80

label_mov_s = [
  movinstr [OpndRM (SIB 1 Nothing Nothing) (DisplLabel "x"),
                                   OpndReg (RegL RegEAX)]] 

loop_jmp_s = [
  HasmStLabel "_start",
  HasmStLabel "loop_start",
  HasmStInstr [] $ Operation OpJmp [OpndRM noSIB (DisplLabel "loop_start")] ]


fromRight = either (error "fromRight $ Left ...") id
