module X86CPU where

import Data.Word
import Data.Int
import Data.Maybe (fromJust)

data GPRegister = RegEAX | RegECX | RegEDX | RegEBX | RegESP | RegEBP | RegESI | RegEDI
                    deriving (Show, Read, Eq)
data GPRegisterW = RegAX | RegCX | RegDX | RegBX | RegSP | RegBP | RegSI | RegDI
                    deriving (Show, Read, Eq)
data GPRegisterB = RegAL | RegCL | RegDL | RegBL
                 | RegAH | RegCH | RegDH | RegBH
                 | RegSPL | RegBPL | RegSIL | RegDIL
               deriving (Show, Read, Eq)
data SegRegister = RegCS | RegSS | RegDS | RegES | RegFS | RegGS
                    deriving (Show, Read, Eq)

allGPRegs = [RegEAX, RegECX, RegEDX, RegEBX, RegESP, RegEBP, RegESI, RegEDI]
allGPWRegs = [RegAX, RegCX, RegDX, RegBX, RegSP, RegBP, RegSI, RegDI]
allGPBRegs = [RegAL, RegCL, RegDL, RegBL, RegAH, RegCH, RegDH, RegBH]
allSegRegs = [RegES, RegCS, RegSS, RegDS, RegFS, RegGS]

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


{-
 - Machine operations
 -}
data Operation
    = OpPush OpOperand
    | OpRet (Maybe Word16)
    | OpLRet (Maybe Word16)
    | OpInt OpOperand
    | OpAdd OpOperand OpOperand
    | OpMov OpOperand OpOperand
    | OpJmp 
  deriving (Show, Read)

data OpPrefix
    = PreLock | PreREP | PreREPE | PreREPZ | PreREPNE | PreREPNZ
    | PreCS | PreSS | PreDS | PreES | PreFS | PreGS
    | PreAddrOverride | PreOffsetOverride
  deriving (Show, Read, Eq)

data OpOperand
    = OpndOffset Jump
    | OpndSegReg SegRegister
    | OpndRegB GPRegisterB | OpndRegW GPRegisterW | OpndReg GPRegister
    | OpndImmB Word8 | OpndImmW Word16 | OpndImmL Word32
    | OpndRM SIB Displacement
 deriving (Show, Read)

data Jump 
    = JDispl8 Int8 | JDispl32 Int32 | JFar Word16 Word32 
    | JIndex Int | JLabel String
  deriving (Show, Read)

data Displacement = NoDispl | Displ8 Int8 | Displ32 Int32
                      deriving (Show, Read)

-- don't use record syntax here because `deriving Read` requires all
-- fields to be explicitly named, it's burdensome.
data SIB = SIB Word8 (Maybe GPRegister) (Maybe GPRegister)
             deriving (Show, Read, Eq)

