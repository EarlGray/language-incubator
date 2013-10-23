module X86Opcodes (
  Operation(..),
  OpOperand(..),
  Serializable(..),
  SIB, Displacement,
) where

import HasmImports
import X86CPU

import Data.Maybe (fromJust)
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
                 ((SIB _ Nothing (Just RegEBP)), NoDispl) ->
                     (useSIB, [0x65, 0x00])
                 -- (%reg):
                 ((SIB _ Nothing (Just reg)), NoDispl) ->
                     (index reg, [])

               -- the same with Displ8:
                 -- $displ8(%esp) : it's a kind of magic again:
                 ((SIB _ Nothing (Just RegESP)), Displ8 dspl8) ->
                     (useSIB .|. useDisplB, [0x24, int dspl8])
                 -- $displ8(%reg)
                 ((SIB _ Nothing (Just reg)), Displ8 dspl8) ->
                     (index reg .|. useDisplB, [int dspl8])

               -- $displ32(%reg)
                 -- $displ32(%esp)
                 ((SIB _ Nothing (Just RegESP)), Displ32 dspl32) ->
                     (useSIB .|. useDisplL, (0x24 : bytecode dspl32))
                 -- $displ32(%reg)
                 ((SIB _ Nothing (Just reg)), Displ32 dspl32) ->
                     (index reg .|. useDisplL, bytecode dspl32)

               -- %esp cannot be index, never:
                 ((SIB _ (Just RegESP) (Just base)), _) ->
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
fourReg = OpndReg RegESP
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
    bytecode imm8 = [int imm8]
instance Serializable Int16 where
    bytecode = unpack . runPut . putWord32le . int
instance Serializable Int32 where
    bytecode = unpack . runPut . putWord32le . int

instance Serializable Operation where
  bytecode (Operation op opnds) =
    case lookup op encoders of
      Just encode -> encode opnds
      Nothing -> error $ "No encoder for operation " ++ show op

encoders = [
  (OpAdd, bytesAdd), (OpMov, bytesMov), (OpPush, bytesPush), 
  (OpRet, bytesRet), (OpLRet, bytesLRet), (OpInt, bytesInt),
  (OpJmp, bytesJmp) ]

-- PUSH
bytesPush :: [OpOperand] -> [Word8]
bytesPush [opnd] = 
  case opnd of 
    OpndImmB immB -> (0x6a : bytecode immB)
    OpndImmW immW -> (preLW : 0x68 : bytecode immW)
    OpndImmL imm  -> (0x68 : bytecode imm)
    OpndReg reg   -> [0x50 + index reg]
    OpndSegReg sReg -> fromJust $ lookup sReg opcodes
      where opcodes = [(RegES, [0x06]), (RegCS, [0x0e]), (RegSS, [0x16]),
                       (RegDS, [0x1e]), (RegFS, [0x0f, 0xa0]), (RegGS, [0x0f, 0xa8])]
    OpndRM _ _    -> (0xff : makeModRM sixReg opnd)
bytesPush _ = []

-- RET, LRET
bytesRet, bytesLRet :: [OpOperand] -> [Word8]
bytesRet [] = [0xc3]
bytesRet [OpndImmW imm] = (0xc2 : bytecode imm)
bytesRet _ = []

bytesLRet [] = [0xcb]
bytesLRet [OpndImmW imm] = (0xca : bytecode imm)
bytesLRet _ = []

-- INT
bytesInt :: [OpOperand] -> [Word8]
bytesInt [OpndImmB imm] 
  | imm == 3    = [0xCC]
  | otherwise   = [0xCD, imm]
bytesInt _ = []

-- ADD
bytesAdd :: [OpOperand] -> [Word8]
bytesAdd [op1, op2] = 
  case (op1, op2) of
    (OpndImmB imm, OpndRegB reg) | reg == RegAL  -> (0x04 : bytecode imm)
    (OpndImmW imm, OpndRegW reg) | reg == RegAX  -> (preLW : 0x05 : bytecode imm)
    (OpndImmL imm, OpndReg reg)  | reg == RegEAX -> (0x05 : bytecode imm)

    (OpndImmB imm, OpndRM _ _) -> [0x80] ++ makeModRM0 op2 ++ bytecode imm
    (OpndImmW imm, OpndRM _ _) -> [preLW, 0x81] ++ makeModRM0 op2 ++ bytecode imm
    (OpndImmL imm, OpndRM _ _) -> [0x81] ++ makeModRM0 op2 ++ bytecode imm
    
    (OpndImmB imm, OpndRegB _) -> [0x80] ++ makeModRM0 op2 ++ bytecode imm
    (OpndImmW imm, OpndRegW _) -> [preLW, 0x81] ++ makeModRM0 op2 ++ bytecode imm
    (OpndImmL imm, OpndReg _)  -> [0x81] ++ makeModRM0 op2 ++ bytecode imm
    
    (OpndImmB imm, OpndReg _)  -> [0x83] ++ makeModRM0 op2 ++ bytecode imm
    (OpndImmB imm, OpndRegW _) -> [preLW, 0x83] ++ makeModRM0 op2 ++ bytecode imm
    
    (OpndReg _,    OpndReg _)  -> (0x01 : makeModRM op1 op2)
    (OpndRegW _,   OpndRegW _) -> (preLW : 0x01 : makeModRM op1 op2)
    (OpndRegB _,   OpndRegB _) -> (0x00 : makeModRM op1 op2)
    
    (OpndReg _,    OpndRM _ _) -> (0x01 : makeModRM op1 op2)
    (OpndRegW _,   OpndRM _ _) -> (preLW : 0x01 : makeModRM op1 op2)
    (OpndRegB _,   OpndRM _ _) -> (0x00 : makeModRM op1 op2)
    
    (OpndRM _ _,   OpndRegB _) -> (0x02 : makeModRM op2 op1)
    (OpndRM _ _,   OpndRegW _) -> (preLW : 0x03 : makeModRM op2 op1)
    (OpndRM _ _,   OpndReg _)  -> (0x03 : makeModRM op2 op1)

bytesAdd _ = []


-- MOV
bytesMov :: [OpOperand] -> [Word8]

bytesMov [op1, op2] = 
  case (op1, op2) of
    (OpndRM NoSIB (DisplAddr moffs), OpndReg RegEAX) -> (0xa1 : bytecode moffs)
    (OpndRM NoSIB (DisplAddr moffs), OpndRegW RegAX) -> (preLW : 0xa1 : bytecode moffs)
    (OpndRM NoSIB (DisplAddr moffs), OpndRegB RegAL) -> (0xa0 : bytecode moffs)

    (OpndReg RegEAX, OpndRM NoSIB (DisplAddr moffs)) -> (0xa3 : bytecode moffs)
    (OpndRegW RegAX, OpndRM NoSIB (DisplAddr moffs)) -> (preLW : 0xa3 : bytecode moffs)
    (OpndRegB RegAL, OpndRM NoSIB (DisplAddr moffs)) -> (0xa2 : bytecode moffs)
    
    (OpndReg _,    OpndReg _)  -> (0x89 : makeModRM op1 op2)
    (OpndRegW _,   OpndRegW _) -> (preLW : 0x89 : makeModRM op1 op2)
    (OpndRegB _,   OpndRegB _) -> (0x88 : makeModRM op1 op2)
    
    (OpndReg _,    OpndRM _ _) -> (0x89 : makeModRM op1 op2)
    (OpndRegW _,   OpndRM _ _) -> (preLW : 0x89 : makeModRM op1 op2)
    (OpndRegB _,   OpndRM _ _) -> (0x88 : makeModRM op1 op2)
    
    (OpndRM _ _,   OpndReg _)  -> (0x8b : makeModRM op2 op1)
    (OpndRM _ _,   OpndRegW _) -> (preLW : 0x8b : makeModRM op2 op1)
    (OpndRM _ _,   OpndRegB _) -> (0x8a : makeModRM op2 op1)
    
    (OpndImmL imm, OpndReg reg)  -> (0xb8 + index reg : bytecode imm)
    (OpndImmW imm, OpndRegW reg) -> (preLW : 0xb8 + index reg : bytecode imm)
    (OpndImmB imm, OpndRegB reg) -> (0xb0 + index reg : bytecode imm)
    
    (OpndImmL imm, OpndRM _ _) -> [0xc6] ++ makeModRM0 op2 ++ bytecode imm
    (OpndImmW imm, OpndRM _ _) -> [preLW, 0xc7] ++ makeModRM0 op2 ++ bytecode imm
    (OpndImmB imm, OpndRM _ _) -> [0xc7] ++ makeModRM0 op2 ++ bytecode imm

    (OpndRegW _,   OpndSegReg _) -> (0x8e : makeModRM op2 op1)
    (OpndSegReg _, OpndRegW _)   -> (0x8c : makeModRM op1 op2)

bytesMov _ = []


-- JMP
bytesJmp :: [OpOperand] -> [Word8]
bytesJmp [op] =
    case op of
      -- treat displacement as offset from EIP
      OpndRM NoSIB (Displ8 moffs8) -> [0xeb, int moffs8]
      OpndRM NoSIB (Displ32 moffs) -> (0xe9 : bytecode (int moffs :: Word16))
      -- treat ModRM/register value as absolute indirect offset
      --  e,g, jmp *0x100000, jmp *%eax, etc
      OpndReg _ -> (0xff : makeModRM fourReg op)
      OpndRM _ _ -> (0xff : makeModRM fourReg op)
bytesJmp _ = []
