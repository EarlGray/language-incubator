module X86Opcodes (
  Operation(..),
  OpOperand(..),
  Serializable(..),
  SIB, Displacement,
) where

import HasmImports
import X86CPU

import Data.Binary.Put (putWord32le, putWord16le, runPut)
import Data.ByteString.Lazy (unpack)
import qualified Data.ByteString as B


-- make ModRM, possible SIB and/or Displacement taken from the second OpOperand
makeModRM :: OpOperand -> OpOperand -> [Word8]

makeModRM (OpndReg lhs) (OpndReg rhs) = [modRM]
  where modRM = 0xC0 .|. (index lhs `shiftL` 3) .|. index rhs
makeModRM (OpndRegW lhs) (OpndRegW rhs) = [modRM]
  where modRM = 0xC0 .|. (index lhs `shiftL` 3) .|. index rhs
makeModRM (OpndRegB lhs) (OpndRegB rhs) = [modRM]
  where modRM = 0xC0 .|. (index lhs `shiftL` 3) .|. index rhs

makeModRM (OpndSegReg sreg) (OpndRegW reg) = [ modRM ]
  where modRM = 0xc0 .|. (index sreg `shiftL` 3) .|. index reg

makeModRM (OpndReg src) (OpndRM sib displ) = [modRM .|. (index src `shiftL` 3) ] ++ sibs
  where (modRM, sibs) =
            case (sib, displ) of
               -- 32-bit memory offset
                 ((SIB _ Nothing Nothing), Displ32 dspl) ->
                     (useAbsDispl, bytecode dspl)

               -- (%reg), NoDisplacement:
                 -- (%ebp): an exception from registers, do a special street magic:
                 ((SIB _ Nothing (Just reg)), NoDispl) | reg == RegEBP ->
                     (useSIB, [0x65, 0x00])
                 -- (%reg):
                 ((SIB _ Nothing (Just reg)), NoDispl) ->
                     (useSIB, [0x20 .|. index reg])

               -- the same with Displ8:
                 -- $displ8(%esp) : it's a kind of magic again:
                 ((SIB _ Nothing (Just reg)), Displ8 dspl8) | reg == RegESP ->
                     (useSIB .|. useDisplB, [0x24, int dspl8])
                 -- $displ8(%reg)
                 ((SIB _ Nothing (Just reg)), Displ8 dspl8) ->
                     (index reg .|. useDisplB, [int dspl8])

               -- $displ32(%reg)
                 -- $displ32(%esp)
                 ((SIB _ Nothing (Just reg)), Displ32 dspl32) | reg == RegESP ->
                     (useSIB .|. useDisplL, (0x24 : bytecode dspl32))
                 -- $displ32(%reg)
                 ((SIB _ Nothing (Just reg)), Displ32 dspl32) ->
                     (index reg .|. useDisplL, bytecode dspl32)

               -- %esp cannot be index, never:
                 ((SIB _ (Just ind) (Just base)), _) | ind == RegESP ->
                     error "%esp cannot be index"
               -- (%ebp,%ind,sc)
                 ((SIB sc (Just ind) (Just base)), NoDispl) | base == RegEBP ->
                     (useSIB .|. useDisplB, [ sibbyte sc ind base, 0x00 ])
               -- (%base,%ind,sc)
                 ((SIB sc (Just ind) (Just base)), NoDispl) ->
                     (useSIB, [sibbyte sc ind base])
               -- $displ8(%base,%ind,sc)
                 ((SIB sc (Just ind) (Just base)), Displ8 dspl8) ->
                     (useSIB .|. useDisplB, (sibbyte sc ind base : bytecode dspl8))
               -- $displ32(%base,%ind,sc)
                 ((SIB sc (Just ind) (Just base)), Displ32 dspl32) ->
                     (useSIB .|. useDisplL, (sibbyte sc ind base : bytecode dspl32))

              -- addressing without base:
                 ((SIB sc (Just ind) Nothing), displ) ->
                     (useSIB, (sibbyte sc ind RegEBP : displbytes))
                     where displbytes = case displ of
                                         NoDispl -> bytecode (0 :: Word32)
                                         Displ8 disp8 -> bytecode (int disp8 :: Word32)
                                         Displ32 disp32 -> bytecode disp32
                 _ -> error $ "This addressing scheme is not supported: " ++ show (OpndRM sib displ)


-- this is a shortcut for /0 operations, RegEAX represented like /0
makeModRM0 :: OpOperand -> [Word8]
makeModRM0 = makeModRM zeroReg

-- these are stubs for embedding numerical value into modRM
--  as required by /0 or /6 commands
zeroReg = OpndReg RegEAX    -- sometimes: use immediate value
sixReg = OpndReg RegEBP     -- use bare displacement

-- modRM flags:
useSIB = 0x04
useAbsDispl = 0x05
useDisplB = 0x40
useDisplL = 0x80
useRegisters = 0xC0

scaling :: Word8 -> Word8
scaling factor = case lookup factor (zip [1,2,4,8] [0,1,2,3]) of
                    Just sc -> sc
                    Nothing -> error "scaling must be one of 1,2,4,8"
sibbyte sc ind base = (scaling sc `shiftL` 6) .|. (index ind `shiftL` 3) .|. index base

{-
 - Serializable: make bytecode from an assembly expression
 -}

class Serializable a where
    bytecode :: a -> [Word8]

instance Serializable OpPrefix where
    bytecode PreCS =    [0x2e]
    bytecode PreSS =    [0x26]
    bytecode PreDS =    [0x3e]
    bytecode PreES =    [0x26]
    bytecode PreFS =    [0x64]
    bytecode PreGS =    [0x65]
    bytecode PreLock =  [0xf0]
    bytecode PreAddrOverride =   [0x66]
    bytecode PreOffsetOverride = [0x67]
    bytecode pre | pre `elem` [PreREPNE, PreREPNZ] = [0xf2]
    bytecode pre | pre `elem` [PreREP, PreREPE, PreREPZ] = [0xf3]

preLW = head $ bytecode PreAddrOverride

instance Serializable Word8 where
    bytecode imm8 = [imm8]
instance Serializable Word16 where
    bytecode = unpack . runPut . putWord16le
instance Serializable Word32 where
    bytecode = unpack . runPut . putWord32le

instance Serializable Int8 where
    bytecode imm8 = [fromIntegral imm8]
instance Serializable Int16 where
    bytecode = unpack . runPut . putWord32le . int
instance Serializable Int32 where
    bytecode = unpack . runPut . putWord32le . int

instance Serializable Operation where
    bytecode (OpPush op) =      bytesPush op
    bytecode (OpRet mbClear) =  bytesRet mbClear
    bytecode (OpLRet mbClear) = bytesLRet mbClear
    bytecode (OpInt op) =       bytesInt op 
    bytecode (OpAdd op1 op2) =  bytesAdd op1 op2
    bytecode (OpMov op1 op2) =  bytesMov op1 op2

-- PUSH
bytesPush :: OpOperand -> [Word8]
bytesPush (OpndSegReg sReg) =
    case lookup sReg opcodes of
      Just bytes -> bytes
    where opcodes = [(RegES, [0x06]), (RegCS, [0x0e]), (RegSS, [0x16]),
                     (RegDS, [0x1e]), (RegFS, [0x0f, 0xa0]), (RegGS, [0x0f, 0xa8])]
bytesPush (OpndImmB imm8) = (0x6a : bytecode imm8)
bytesPush (OpndImmL imm) = (0x68 : bytecode imm)
bytesPush (OpndReg reg) = [0x50 + index reg]
bytesPush oprm@(OpndRM _ _) = (0xff : makeModRM sixReg oprm)

-- RET, LRET
bytesRet, bytesLRet :: (Maybe Word16) -> [Word8]
bytesRet (Just imm16) = (0xc2 : bytecode imm16)
bytesRet _            = [0xc3]
bytesLRet (Just imm16) = (0xca : bytecode imm16)
bytesLRet _            = [0xcb]

-- INT
bytesInt :: OpOperand -> [Word8]
bytesInt (OpndImmB imm) | imm == 3  = [0xCC]
bytesInt (OpndImmB imm) = [0xCD, imm]
bytesInt _ = error "Invalid operand form for INT"

-- ADD
bytesAdd :: OpOperand -> OpOperand -> [Word8]
bytesAdd (OpndImmB imm) (OpndRegB reg) | reg == RegAL  = (0x04 : bytecode imm)
bytesAdd (OpndImmW imm) (OpndRegW reg) | reg == RegAX  = (preLW : 0x05 : bytecode imm)
bytesAdd (OpndImmL imm) (OpndReg reg) | reg == RegEAX  = (0x05 : bytecode imm)

bytesAdd (OpndImmB imm)   op2@(OpndRM _ _) = [0x80] ++ makeModRM0 op2 ++ bytecode imm
bytesAdd (OpndImmW imm)   op2@(OpndRM _ _) = [preLW, 0x81] ++ makeModRM0 op2 ++ bytecode imm
bytesAdd (OpndImmL imm)   op2@(OpndRM _ _) = [0x81] ++ makeModRM0 op2 ++ bytecode imm

bytesAdd (OpndImmB imm) opreg@(OpndRegB _) = [0x80] ++ makeModRM0 opreg ++ bytecode imm
bytesAdd (OpndImmW imm) opreg@(OpndRegW _) = [preLW, 0x81] ++ makeModRM0 opreg ++ bytecode imm
bytesAdd (OpndImmL imm) opreg@(OpndReg _) = [0x81] ++ makeModRM0 opreg ++ bytecode imm

bytesAdd (OpndImmB imm) opreg@(OpndReg _) = [0x83] ++ makeModRM0 opreg ++ bytecode imm
bytesAdd (OpndImmB imm) opreg@(OpndRegW _) = [preLW, 0x83] ++ makeModRM0 opreg ++ bytecode imm

bytesAdd op1@(OpndReg _)    op2@(OpndReg _) = (0x01 : makeModRM op1 op2)
bytesAdd op1@(OpndRegW _)   op2@(OpndRegW _) = (preLW : 0x01 : makeModRM op1 op2)
bytesAdd op1@(OpndRegB _)   op2@(OpndRegB _) = (0x00 : makeModRM op1 op2)

bytesAdd op1@(OpndReg _)    op2@(OpndRM _ _) = (0x01 : makeModRM op1 op2)
bytesAdd op1@(OpndRegW _)   op2@(OpndRM _ _) = (preLW : 0x01 : makeModRM op1 op2)
bytesAdd op1@(OpndRegB _)   op2@(OpndRM _ _) = (0x00 : makeModRM op1 op2)

bytesAdd op1@(OpndRM _ _)   op2@(OpndRegB _) = (0x02 : makeModRM op2 op1)
bytesAdd op1@(OpndRM _ _)   op2@(OpndRegW _) = (preLW : 0x03 : makeModRM op2 op1)
bytesAdd op1@(OpndRM _ _)   op2@(OpndReg _)  = (0x03 : makeModRM op2 op1)

bytesAdd _ _ = error "ADD: invalid operands"


-- MOV
bytesMov :: OpOperand -> OpOperand -> [Word8]

bytesMov (OpndMem moffs) (OpndReg reg) | reg == RegEAX = (0xa1 : bytecode moffs)
bytesMov (OpndMem moffs) (OpndRegW reg) | reg == RegAX = (preLW : 0xa1 : bytecode moffs)
bytesMov (OpndMem moffs) (OpndRegB reg) | reg == RegAL = (0xa0 : bytecode moffs)

bytesMov (OpndReg reg) (OpndMem moffs) | reg == RegEAX = (0xa3 : bytecode moffs)
bytesMov (OpndRegW reg) (OpndMem moffs) | reg == RegAX = (preLW : 0xa3 : bytecode moffs)
bytesMov (OpndRegB reg) (OpndMem moffs) | reg == RegAL = (0xa2 : bytecode moffs)

bytesMov op1@(OpndReg _)    op2@(OpndReg _) = (0x89 : makeModRM op1 op2)
bytesMov op1@(OpndRegW _)   op2@(OpndRegW _) = (preLW : 0x89 : makeModRM op1 op2)
bytesMov op1@(OpndRegB _)   op2@(OpndRegB _) = (0x88 : makeModRM op1 op2)

bytesMov op1@(OpndReg _)    op2@(OpndRM _ _) = (0x89 : makeModRM op1 op2)
bytesMov op1@(OpndRegW _)   op2@(OpndRM _ _) = (preLW : 0x89 : makeModRM op1 op2)
bytesMov op1@(OpndRegB _)   op2@(OpndRM _ _) = (0x88 : makeModRM op1 op2)

bytesMov op2@(OpndRM _ _)   op1@(OpndReg _)    = (0x8b : makeModRM op1 op2)
bytesMov op2@(OpndRM _ _)   op1@(OpndRegW _)   = (preLW : 0x8b : makeModRM op1 op2)
bytesMov op2@(OpndRM _ _)   op1@(OpndRegB _)   = (0x8a : makeModRM op1 op2)

bytesMov (OpndImmL imm) (OpndReg reg) = (0xb8 + index reg : bytecode imm)
bytesMov (OpndImmW imm) (OpndRegW reg) = (preLW : 0xb8 + index reg : bytecode imm)
bytesMov (OpndImmB imm) (OpndRegB reg) = (0xb0 + index reg : bytecode imm)

bytesMov (OpndImmL imm) oprm@(OpndRM _ _) = [0xc6] ++ makeModRM0 oprm ++ bytecode imm
bytesMov (OpndImmW imm) oprm@(OpndRM _ _) = [preLW, 0xc7] ++ makeModRM0 oprm ++ bytecode imm
bytesMov (OpndImmB imm) oprm@(OpndRM _ _) = [0xc7] ++ makeModRM0 oprm ++ bytecode imm

bytesMov opr@(OpndRegW _) opsr@(OpndSegReg _) = (0x8e : makeModRM opsr opr)
bytesMov opsr@(OpndSegReg _) opr@(OpndRegW _) = (0x8c : makeModRM opsr opr)

bytesMov _ _ = error "Invalid operands"


-- JMP
bytesJmp :: OpOperand -> [Word8]
bytesJmp (OpndOffset jump) =
    case jump of
      JDispl8 d8 -> [0xeb, int d8]
      JDispl32 d32 -> (0xe9 : bytecode d32)
bytesJmp _ = error "Invalid operands"
