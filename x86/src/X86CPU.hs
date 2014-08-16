module X86CPU where

import Data.Word
import Data.Int
import Data.Maybe (fromJust)

data ImmValue = ImmL Word32 | ImmW Word16 | ImmB Word8 deriving (Show, Read, Eq)

data Register
    = RegL GPRegister | RegW GPRegisterW
    | RegB GPRegisterB | SReg SegRegister
  deriving (Show, Read, Eq)

data GPRegister
    = RegEAX | RegECX | RegEDX | RegEBX
    | RegESP | RegEBP | RegESI | RegEDI
  deriving (Show, Read, Eq)
data GPRegisterW
    = RegAX | RegCX | RegDX | RegBX
    | RegSP | RegBP | RegSI | RegDI
  deriving (Show, Read, Eq)
data GPRegisterB
    = RegAL | RegCL | RegDL | RegBL
    | RegAH | RegCH | RegDH | RegBH
    | RegSPL | RegBPL | RegSIL | RegDIL
  deriving (Show, Read, Eq)
data SegRegister
    = RegCS | RegSS
    | RegDS | RegES | RegFS | RegGS
  deriving (Show, Read, Eq)

allGPRegs = [RegEAX, RegECX, RegEDX, RegEBX, RegESP, RegEBP, RegESI, RegEDI]
allGPWRegs = [RegAX, RegCX, RegDX, RegBX, RegSP, RegBP, RegSI, RegDI]
allGPBRegs = [RegAL, RegCL, RegDL, RegBL, RegAH, RegCH, RegDH, RegBH]
allSegRegs = [RegES, RegCS, RegSS, RegDS, RegFS, RegGS]

gpRegNames = ["eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi"]
gpWRegNames = ["ax", "cx", "dx", "bx", "sp", "bp", "si", "di"]
gpBRegNames = ["al", "cl", "dl", "bl", "ah", "ch", "dh", "bh"]
segRegNames = ["es", "cs", "ss", "ds", "fs", "gs"]

lkupRegL = zip gpRegNames allGPRegs
lkupRegW = zip gpWRegNames allGPWRegs
lkupRegB = zip gpBRegNames allGPBRegs
lkupSReg = zip segRegNames allSegRegs

mbRegByName :: String -> Maybe Register
mbRegByName rname =
  case lookup rname lkupRegL of
    Just reg -> Just $ RegL reg
    Nothing ->
      case lookup rname lkupRegW of
        Just reg -> Just $ RegW reg
        Nothing ->
          case lookup rname lkupRegB of
            Just reg -> Just $ RegB reg
            Nothing ->
              case lookup rname lkupSReg of
                Just reg -> Just $ SReg reg
                Nothing -> Nothing

{-
 - Indexable: enumerations to bit representation
 -}
class Indexable a where
    index :: a -> Word8

instance Indexable GPRegister where
    index reg = fromJust $ lookup reg (zip allGPRegs [0..7])
instance Indexable GPRegisterW where
    index reg = fromJust $ lookup reg (zip allGPWRegs [0..7])
instance Indexable GPRegisterB where
    index reg = case lookup reg (zip allGPBRegs [0..7]) of
                    Just ind -> ind
                    Nothing -> error "SPL/BPL/SIL/DIL registers can't be indexed"
instance Indexable SegRegister where
    index reg = fromJust $ lookup reg (zip allSegRegs [0..5])

indexOfReg :: Register -> Word8
indexOfReg (RegL reg) = index reg
indexOfReg (RegW reg) = index reg
indexOfReg (RegB reg) = index reg
indexOfReg (SReg sr) = index sr

{-
 - Machine operations
 -}
type Symbol = String

data Instr = OpPush | OpRet | OpLRet | OpInt | OpAdd | OpMov | OpJmp
           | OpCmp  | OpJe  | OpJne  | OpJa  | OpJna | OpJnae| OpJge 
           | OpJae  | OpJl  | OpJle  | OpJg  | OpJnp | OpJp  | OpJno  
           | OpJo   | OpJs  | OpJns  | OpJc  | OpJnc | OpJbe | OpJecxz
  deriving (Show, Read, Eq)

data Operation = Operation Instr [OpOperand]
  deriving (Show, Read, Eq)

data OpPrefix
    = PreLock | PreREP | PreREPE | PreREPZ | PreREPNE | PreREPNZ
    | PreCS | PreSS | PreDS | PreES | PreFS | PreGS
    | PreAddrOverride | PreOffsetOverride
  deriving (Show, Read, Eq)

data OpOperand
    = OpndReg Register
    | OpndImm ImmValue
    | OpndRM SIB Displacement
 deriving (Show, Read, Eq)

data Displacement
    = NoDispl
    | Displ8 Word8
    | Displ32 Word32
    | DisplLabel Symbol
  deriving (Show, Read, Eq)

-- don't use record syntax here because `deriving Read` requires all
-- fields to be explicitly named, it's burdensome.
data SIB = SIB Word8 (Maybe GPRegister) (Maybe GPRegister)
             deriving (Show, Read, Eq)

noSIB = SIB 1 Nothing Nothing
