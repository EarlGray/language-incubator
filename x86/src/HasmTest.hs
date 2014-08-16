module Main where

import HasmTypes
import HasmParse
import HasmCodegen
import HasmPretty

{-
 -  Test Examples
 -}
regl = OpndReg . RegL
imml = OpndImm . ImmL
immb = OpndImm . ImmB
label l = OpndRM noSIB (DisplLabel l)

instr i = HasmStInstr [] . Operation i

regdspl b d = OpndRM (SIB 1 Nothing (Just b)) d

linux_null_s = [
  HasmStLabel "_start",                 -- _start:
  instr OpMov [imml 0x0, regl RegEAX],  --    movl $1, %eax
  instr OpMov [imml 0x1, regl RegEBX],  --    movl $0, %ebx
  instr OpInt [immb 0x80] ]             --    int 0x80

label_mov_s = [
  instr OpMov [label "x", regl RegEAX]]

loop_jmp_s = [
  HasmStLabel "_start",
  HasmStLabel "loop_start",
  instr OpJmp [label "loop_start"] ]

{-
factorial_s = [
  instr OpMov [regdspl RegESP (Displ8 4), regl RegECX],
  instr OpCmp [imml 1, regl RegECX],
  instr OpJE  [label "lbl1"],
  instr OpMov [imml 1, regl RegEAX],
  instr OpMov [imml 1, regl RegEDX],
  HasmStLabel "lbl2",
  instr OpIMul [regl RegEDX, regl RegEAX],
  instr OpAdd [imml 1, regl RegEDX],
  instr OpCmp [regl RegECX, regl RegEDX],
  instr OpJne [label "lbl2"],
  instr OpRet [],
  HasmStLabel "lbl1",
  instr OpMov [imml 1, regl RegEAX],
  instr OpRet [] ]
-}

fromRight = either (error "fromRight $ Left ...") id

assembleStmts :: [HasmStatement] -> [(HasmStatement, SrcPos, [Word8])]
assembleStmts = fromRight . assembleFromZero . withTestSrc

