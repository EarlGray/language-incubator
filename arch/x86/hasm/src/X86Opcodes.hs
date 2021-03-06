module X86Opcodes (
  Operation(..),
  OpOperand(..),
  Serializable(..),
  bytecodeWithPos,
  Symbol,
  SIB, Displacement,
) where

import X86CPU
import HasmTypes

import Data.Word
import Data.Int
import Data.Bits

import Data.List (genericLength)
import Data.Maybe (fromJust)
import Data.Binary.Put (putWord32le, putWord16le, runPut)
import Data.ByteString.Lazy (unpack)
import qualified Data.ByteString as B

srcModRM :: Register -> [Word8] -> [Word8]
srcModRM reg (modrm : rest) = (((index reg `shiftL` 3) .|. modrm) : rest)

-- make ModRM, possible SIB and/or Displacement taken from the second OpOperand
makeModRM :: OpOperand -> OpOperand -> [Word8]
makeModRM (OpndReg src) dst = srcModRM src $ makeModRM0 dst

-- makeModRM just for dst
makeModRM0 :: OpOperand -> [Word8]

makeModRM0 (OpndReg reg) = [ 0xC0 .|. index reg ]
makeModRM0 (OpndRM sib displ) =
      case (sib, displ) of
        -- 32-bit memory offset
        (noSIB, Displ32 dspl) ->
            (useAbsDispl : bytecode dspl)

        -- (%reg), NoDisplacement:
        -- (%reg):
        ((SIB _ Nothing (Just reg)), NoDispl) ->
          case reg of
            -- (%ebp): an exception from registers, do a special street magic:
            RegEBP -> makeModRM0 (OpndRM (SIB 1 Nothing (Just RegEBP)) (Displ8 0))
            -- (%esp): must be encoded with SIB
            RegESP -> [useSIB, sibNoIndex .|. index RegESP]
            _ -> [index reg]

        -- the same with Displ8:
        -- $displ8(%reg)
        ((SIB _ Nothing (Just reg)), Displ8 dspl8) ->
            if reg == RegESP
            -- $displ8(%esp) : it's a kind of magic again:
            then [useSIB .|. useDisplB, sibNoIndex .|. index RegESP, int dspl8]
            else [index reg .|. useDisplB, int dspl8]

        -- $displ32(%reg)
        -- $displ32(%esp)
        ((SIB _ Nothing (Just reg)), Displ32 dspl32) ->
            if reg == RegESP
            then ((useSIB .|. useDisplL) : 0x24 : bytecode dspl32)
            else ((index reg .|. useDisplL) : bytecode dspl32)

        -- %esp cannot be index, never:
        ((SIB _ (Just RegESP) (Just base)), _) ->
            error "%esp cannot be index"
        -- (%ebp,%ind,sc) -- yet another exception
        ((SIB sc (Just ind) (Just RegEBP)), NoDispl) ->
            makeModRM0 (OpndRM (SIB sc (Just ind) (Just RegEBP)) (Displ8 0))
        -- (%base,%ind,sc)
        ((SIB sc (Just ind) (Just base)), NoDispl) ->
            [useSIB, sibbyte sc ind base]
        -- $displ8(%base,%ind,sc)
        ((SIB sc (Just ind) (Just base)), Displ8 dspl8) ->
            ((useSIB .|. useDisplB) : sibbyte sc ind base : bytecode dspl8)
        -- $displ32(%base,%ind,sc)
        ((SIB sc (Just ind) (Just base)), Displ32 dspl32) ->
            ((useSIB .|. useDisplL) : sibbyte sc ind base : bytecode dspl32)

        -- addressing without base:
        ((SIB sc (Just ind) Nothing), displ) ->
            (useSIB : sibbyte sc ind RegEBP : displbytes)
            where displbytes = case displ of
                                NoDispl -> bytecode (0 :: Word32)
                                Displ8 disp8 -> bytecode (int disp8 :: Word32)
                                Displ32 disp32 -> bytecode disp32
        _ -> error $ "This addressing scheme is not supported: " ++ show (OpndRM sib displ)


-- these are stubs for embedding numerical value into modRM
--  as required by, e.g. /0 or /6 commands
stubRM n = OpndReg (RegL (toEnum n :: GPRegister))

-- modRM flags:
useSIB = 0x04
useAbsDispl = 0x05
useDisplB = 0x40
useDisplL = 0x80
useRegisters = 0xC0

-- sib
sibNoIndex = 0x20

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

bytecodeWithPos :: Operation -> Word32 -> [Word8]
bytecodeWithPos oper@(Operation op opnds) addr =
    case lookup op positionAwareEncoders of
      Just encoder -> encoder addr opnds
      _ -> bytecode oper

encoders = [
  (OpAdd, bytesAdd), (OpMov, bytesMov), (OpPush, bytesPush),
  (OpRet, bytesRet), (OpLRet, bytesLRet), (OpInt, bytesInt),
  (OpCmp, bytesCmp), (OpIMul, bytesIMul) ]

positionAwareEncoders = [
  (OpJmp, bytesJmp),
  (OpJa,  bytesJc [0x77] [0x0f, 0x87]), (OpJna,  bytesJc [0x76] [0x0f, 0x86]),
  (OpJc,  bytesJc [0x72] [0x0f, 0x82]), (OpJnc,  bytesJc [0x73] [0x0f, 0x83]),
  (OpJe,  bytesJc [0x74] [0x0f, 0x84]), (OpJne,  bytesJc [0x75] [0x0f, 0x85]),
  (OpJl,  bytesJc [0x7c] [0x0f, 0x8c]), (OpJle,  bytesJc [0x7e] [0x0f, 0x8e]),
  (OpJg,  bytesJc [0x7f] [0x0f, 0x8f]), (OpJge,  bytesJc [0x7d] [0x0f, 0x8d]),
  (OpJo,  bytesJc [0x70] [0x0f, 0x80]), (OpJno,  bytesJc [0x71] [0x0f, 0x81]),
  (OpJp,  bytesJc [0x7a] [0x0f, 0x8a]), (OpJnp,  bytesJc [0x7b] [0x0f, 0x8b]),
  (OpJs,  bytesJc [0x78] [0x0f, 0x88]), (OpJns,  bytesJc [0x79] [0x0f, 0x89]),
  (OpJbe, bytesJc [0x76] [0x0f, 0x86]), (OpJecxz, bytesJc [0xe3] []) ]

-- JE, JA, JAE, JC, J..
bytesJc bs8 bs32 addr op =
  case op of
    [OpndRM noSIB (Displ8 moff)]  -> bs8 ++ bytecode moff
    [OpndRM noSIB (Displ32 moff)] ->
        let rel32 = moff - (addr + genericLength bs32 + 4)  -- 4 == length (bytecode displ32)
        in bs32 ++ bytecode rel32
    _ -> []

-- PUSH
bytesPush :: [OpOperand] -> [Word8]
bytesPush [opnd] =
  case opnd of
    OpndImm imm ->
      case imm of
        ImmB immB -> (0x6a : bytecode immB)
        ImmW immW -> (preLW : 0x68 : bytecode immW)
        ImmL imm  -> (0x68 : bytecode imm)
    OpndReg reg   ->
      case reg of
        RegW reg -> [preLW, 0x50 + index reg]
        RegL reg -> [0x50 + index reg]
        RegB reg -> error "One-byte register cannot be argument for push"
        SReg reg -> fromJust $ lookup reg opcodes
            where opcodes =
                    [(RegES, [0x06]), (RegCS, [0x0e]), (RegSS, [0x16]),
                     (RegDS, [0x1e]), (RegFS, [0x0f, 0xa0]), (RegGS, [0x0f, 0xa8])]
    OpndRM _ _    -> (0xff : makeModRM (stubRM 6) opnd)
bytesPush _ = []

-- RET, LRET
bytesRet, bytesLRet :: [OpOperand] -> [Word8]
bytesRet [] = [0xc3]
bytesRet [OpndImm (ImmW imm)] = (0xc2 : bytecode imm)
bytesRet _ = []

bytesLRet [] = [0xcb]
bytesLRet [OpndImm (ImmW imm)] = (0xca : bytecode imm)
bytesLRet _ = []

-- INT
bytesInt :: [OpOperand] -> [Word8]
bytesInt [OpndImm imm] =
  case imm of
    ImmB 3    -> [0xCC]
    ImmB immb -> [0xCD, immb]
    ImmL imm  -> [0xCD, int imm]
    _ -> error $ "Failed to assemble `int " ++ show imm
bytesInt _ = []

-- ADD
bytesAdd :: [OpOperand] -> [Word8]
bytesAdd [op1, op2] =
  case (op1, op2) of
    (OpndImm (ImmB imm), OpndReg (RegB RegAL))  -> (0x04 : bytecode imm)
    (OpndImm (ImmW imm), OpndReg (RegW RegAX))  -> (preLW : 0x05 : bytecode imm)
    (OpndImm (ImmL imm), OpndReg (RegL RegEAX)) -> (0x05 : bytecode imm)

    (OpndImm (ImmB imm), OpndRM _ _) -> [0x80] ++ makeModRM0 op2 ++ bytecode imm
    (OpndImm iv@(ImmW imm), OpndRM _ _) ->
        if (isWord8 iv)
        then (preLW : 0x83 : makeModRM0 op2) ++ bytecode (int imm :: Word8)
        else (preLW : 0x81 : makeModRM0 op2) ++ bytecode imm
    (OpndImm iv@(ImmL imm), OpndRM _ _) ->
        if (isWord8 iv)
        then (0x83 : makeModRM0 op2) ++ bytecode (int imm :: Word8)
        else (0x81 : makeModRM0 op2) ++ bytecode imm

    (OpndImm (ImmB imm), OpndReg (RegB _)) -> [0x80] ++ makeModRM0 op2 ++ bytecode imm
    (OpndImm iv@(ImmW imm), OpndReg (RegW _)) ->
        if (isWord8 iv)
        then (preLW : 0x83 : makeModRM0 op2) ++ bytecode (int imm :: Word8)
        else (preLW : 0x81 : makeModRM0 op2) ++ bytecode imm
    (OpndImm iv@(ImmL imm), OpndReg (RegL _)) ->
        if (isWord8 iv)
        then (0x83 : makeModRM0 op2) ++ bytecode (int imm :: Word8)
        else (0x81 : makeModRM0 op2) ++ bytecode imm

    -- sign-extend:
    (OpndImm (ImmB imm), OpndReg (RegL _)) -> [0x83] ++ makeModRM0 op2 ++ bytecode imm
    (OpndImm (ImmB imm), OpndReg (RegW _)) -> [preLW, 0x83] ++ makeModRM0 op2 ++ bytecode imm

    (OpndReg (RegL _), OpndReg (RegL _))  -> (0x01 : makeModRM op1 op2)
    (OpndReg (RegW _), OpndReg (RegW _)) -> (preLW : 0x01 : makeModRM op1 op2)
    (OpndReg (RegB _), OpndReg (RegB _)) -> (0x00 : makeModRM op1 op2)

    (OpndReg (RegL _), OpndRM _ _) -> (0x01 : makeModRM op1 op2)
    (OpndReg (RegW _), OpndRM _ _) -> (preLW : 0x01 : makeModRM op1 op2)
    (OpndReg (RegB _), OpndRM _ _) -> (0x00 : makeModRM op1 op2)

    (OpndRM _ _,   OpndReg (RegB _)) -> (0x02 : makeModRM op2 op1)
    (OpndRM _ _,   OpndReg (RegW _)) -> (preLW : 0x03 : makeModRM op2 op1)
    (OpndRM _ _,   OpndReg (RegL _)) -> (0x03 : makeModRM op2 op1)

    _ -> error $ "failed to assemble operands: " ++ show op1 ++ ", " ++ show op2

bytesAdd _ = []


-- IMUL, signed multiplication
bytesIMul :: [OpOperand] -> [Word8]
bytesIMul ops =
  case ops of
    [op1] ->
      case op1 of
        --OpndRM _ _ -> (0xf6 : makeModRM (stubRM 5) op1)   -- if r/m8
        --OpndRM _ _ -> (preLW : 0xf7 : makeModRM (stubRM 5) op1)
        OpndRM _ _ -> (0xf7 : makeModRM (stubRM 5) op1)
        _ -> []

    [op1, op2] ->
      case (op1, op2) of
        (OpndRM _ _,    OpndReg (RegW _)) ->
            (preLW : 0x0f : 0xaf : makeModRM op2 op1)
        (OpndRM _ _,    OpndReg (RegL _)) ->
            (0x0f : 0xaf : makeModRM op2 op1)
        (OpndReg (RegW _),    OpndReg (RegW _)) ->
            (preLW : 0x0f : 0xaf : makeModRM op2 op1)
        (OpndReg (RegL _),    OpndReg (RegL _)) ->
            (0x0f : 0xaf : makeModRM op2 op1)
        _ -> []

    [op1, op2, op3] ->
      case (op1, op2, op3) of
        (OpndImm (ImmB imm),  OpndRM _ _, OpndReg (RegW _)) ->
            (preLW : 0x6b : makeModRM op3 op2) ++ bytecode imm
        (OpndImm (ImmB imm),  OpndRM _ _, OpndReg (RegL _)) ->
            (0x6b : makeModRM op3 op2) ++ bytecode imm
        (OpndImm (ImmW imm), OpndRM _ _, OpndReg (RegW _)) ->
            (preLW : 0x69 : makeModRM op3 op2) ++ bytecode imm
        (OpndImm (ImmL imm), OpndRM _ _, OpndReg (RegL _)) ->
            (0x69 : makeModRM op3 op2) ++ bytecode imm
        _ -> []
    _ -> error $ "failed to assemble operands: " ++ show ops

-- MOV
bytesMov :: [OpOperand] -> [Word8]

bytesMov [op1, op2] =
  case (op1, op2) of
    (OpndRM noSIB (Displ32 moffs), OpndReg (RegL RegEAX)) -> (0xa1 : bytecode moffs)
    (OpndRM noSIB (Displ32 moffs), OpndReg (RegW RegAX)) -> (preLW : 0xa1 : bytecode moffs)
    (OpndRM noSIB (Displ32 moffs), OpndReg (RegB RegAL)) -> (0xa0 : bytecode moffs)

    (OpndReg (RegL RegEAX), OpndRM noSIB (Displ32 moffs)) -> (0xa3 : bytecode moffs)
    (OpndReg (RegW RegAX),  OpndRM noSIB (Displ32 moffs)) -> (preLW : 0xa3 : bytecode moffs)
    (OpndReg (RegB RegAL),  OpndRM noSIB (Displ32 moffs)) -> (0xa2 : bytecode moffs)

    (OpndReg (RegL _), OpndReg (RegL _)) -> (0x89 : makeModRM op1 op2)
    (OpndReg (RegW _), OpndReg (RegW _)) -> (preLW : 0x89 : makeModRM op1 op2)
    (OpndReg (RegB _), OpndReg (RegB _)) -> (0x88 : makeModRM op1 op2)

    (OpndReg (RegL _), OpndRM _ _) -> (0x89 : makeModRM op1 op2)
    (OpndReg (RegW _), OpndRM _ _) -> (preLW : 0x89 : makeModRM op1 op2)
    (OpndReg (RegB _), OpndRM _ _) -> (0x88 : makeModRM op1 op2)

    (OpndRM _ _,   OpndReg (RegL _))  -> (0x8b : makeModRM op2 op1)
    (OpndRM _ _,   OpndReg (RegW _)) -> (preLW : 0x8b : makeModRM op2 op1)
    (OpndRM _ _,   OpndReg (RegB _)) -> (0x8a : makeModRM op2 op1)

    (OpndImm (ImmL imm), OpndReg (RegL reg)) -> (0xb8 + index reg : bytecode imm)
    (OpndImm (ImmW imm), OpndReg (RegW reg)) -> (preLW : 0xb8 + index reg : bytecode imm)
    (OpndImm (ImmB imm), OpndReg (RegB reg)) -> (0xb0 + index reg : bytecode imm)

    (OpndImm (ImmL imm), OpndRM _ _) -> [0xc6] ++ makeModRM0 op2 ++ bytecode imm
    (OpndImm (ImmW imm), OpndRM _ _) -> [preLW, 0xc7] ++ makeModRM0 op2 ++ bytecode imm
    (OpndImm (ImmB imm), OpndRM _ _) -> [0xc7] ++ makeModRM0 op2 ++ bytecode imm

    (OpndReg (RegW _), OpndReg (SReg _)) -> (0x8e : makeModRM op2 op1)
    (OpndRM _ _,       OpndReg (SReg _)) -> (0x8e : makeModRM op2 op1)
    (OpndReg (SReg _), OpndReg (RegW _)) -> (0x8c : makeModRM op1 op2)
    (OpndReg (SReg _), OpndRM _ _)       -> (0x8c : makeModRM op1 op2)

    _ -> error $ "failed to assemble operands: " ++ show op1 ++ ", " ++ show op2

bytesMov _ = []

-- CMP
bytesCmp :: [OpOperand] -> [Word8]
bytesCmp [op1, op2] =
  case (op1, op2) of
    (OpndImm (ImmB imm), OpndReg (RegB RegAL))      -> (0x3c : bytecode imm)
    (OpndImm (ImmW imm), OpndReg (RegW RegAX))      -> (preLW : 0x3d : bytecode imm)
    (OpndImm (ImmL imm), OpndReg (RegL RegEAX))     -> (0x3d : bytecode imm)

    (OpndImm (ImmB imm), OpndReg (RegB _)) -> (0x80 : makeModRM (stubRM 7) op2) ++ bytecode imm
    (OpndImm iv@(ImmW imm), OpndReg (RegW _)) ->
        if (isWord8 iv)
        then (preLW : 0x83 : makeModRM (stubRM 7) op2) ++ bytecode (int imm :: Word8)
        else (preLW : 0x81 : makeModRM (stubRM 7) op2) ++ bytecode imm
    (OpndImm iv@(ImmL imm), OpndReg (RegL _)) ->
        if (isWord8 iv)
        then (0x83 : makeModRM (stubRM 7) op2) ++ bytecode (int imm :: Word8)
        else (0x81 : makeModRM (stubRM 7) op2) ++ bytecode imm

    (OpndImm (ImmB imm), OpndRM _ _) -> (0x80 : makeModRM (stubRM 7) op2) ++ bytecode imm
    (OpndImm iv@(ImmW imm), OpndRM _ _) ->
        if (isWord8 iv)
        then (preLW : 0x83 : makeModRM (stubRM 7) op2) ++ bytecode (int imm :: Word8)
        else (preLW : 0x81 : makeModRM (stubRM 7) op2) ++ bytecode imm
    (OpndImm iv@(ImmL imm), OpndRM _ _) ->
        if (isWord8 iv)
        then (0x83 : makeModRM (stubRM 7) op2) ++ bytecode (int imm :: Word8)
        else (0x81 : makeModRM (stubRM 7) op2) ++ bytecode imm

    (OpndReg (RegB _),  OpndReg (RegB _))  -> (0x38 : makeModRM op1 op2)
    (OpndReg (RegW _),  OpndReg (RegW _))  -> (preLW : 0x39 : makeModRM op1 op2)
    (OpndReg (RegL _),  OpndReg (RegL _))  -> (0x39 : makeModRM op1 op2)

    (OpndReg (RegB _),  OpndRM _ _)  -> (0x38 : makeModRM op1 op2)
    (OpndReg (RegW _),  OpndRM _ _)  -> (preLW : 0x39 : makeModRM op1 op2)
    (OpndReg (RegL _),  OpndRM _ _)  -> (0x39 : makeModRM op1 op2)

    (OpndRM _ _,  OpndReg (RegB _))  -> (0x3a : makeModRM op2 op1)
    (OpndRM _ _,  OpndReg (RegW _))  -> (preLW : 0x3b : makeModRM op2 op1)
    (OpndRM _ _,  OpndReg (RegL _))  -> (0x3b : makeModRM op2 op1)

    _ -> error $ "failed to assemble operands: " ++ show op1 ++ ", " ++ show op2
bytesCmp _ = []

-- JMP
bytesJmp :: Word32 -> [OpOperand] -> [Word8]
bytesJmp addr [op] =
    case op of
      -- treat displacement as offset from EIP
      OpndRM noSIB (Displ8 moffs8) -> (0xeb : bytecode moffs8)
      OpndRM noSIB (Displ32 moffs) ->
        let rel32 = (moffs - (addr + 5))    -- 5 == length (0xe9 : displ32)
        in (0xe9 : bytecode rel32)
      -- treat ModRM/register value as absolute indirect offset
      --  e,g, jmp *0x100000, jmp *%eax, etc
      OpndRM _ _ -> (0xff : makeModRM (stubRM 4) op)
      OpndReg _ -> (0xff : makeModRM (stubRM 4) op)
bytesJmp _ _ = []

