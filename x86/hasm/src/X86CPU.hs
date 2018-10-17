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
  deriving (Show, Read, Eq, Enum)
data GPRegisterW
    = RegAX | RegCX | RegDX | RegBX
    | RegSP | RegBP | RegSI | RegDI
  deriving (Show, Read, Eq, Enum)
data GPRegisterB
    = RegAL | RegCL | RegDL | RegBL
    | RegAH | RegCH | RegDH | RegBH
    | RegSPL | RegBPL | RegSIL | RegDIL
  deriving (Show, Read, Eq, Enum)
data SegRegister
    = RegES | RegCS | RegSS | RegDS | RegFS | RegGS
  deriving (Show, Read, Eq, Enum)

enumAll :: Enum a => [a]
enumAll = enumFrom (toEnum 0)

gpRegNames = ["eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi"]
gpWRegNames = ["ax", "cx", "dx", "bx", "sp", "bp", "si", "di"]
gpBRegNames = ["al", "cl", "dl", "bl", "ah", "ch", "dh", "bh"]
segRegNames = ["es", "cs", "ss", "ds", "fs", "gs"]

lkupRegL = zip gpRegNames  (enumAll :: [GPRegister])
lkupRegW = zip gpWRegNames (enumAll :: [GPRegisterW])
lkupRegB = zip gpBRegNames (enumAll :: [GPRegisterB])
lkupSReg = zip segRegNames (enumAll :: [SegRegister])

mbRegByName :: String -> Maybe Register
mbRegByName rname =
  let onSnd f (a,b) = (a, f b)
      wrap with tbl = map (onSnd with) tbl
      regls = wrap RegL lkupRegL
      regws = wrap RegW lkupRegW
      regbs = wrap RegB lkupRegB
      sregs = wrap SReg lkupSReg
  in lookup rname $ concat [regls, regws, regbs, sregs]

{-
 - Indexable: enumerations to bit representation
 -}
class Indexable a where
    index :: a -> Word8

instance Indexable GPRegister where
    index reg = fromIntegral $ fromEnum reg
instance Indexable GPRegisterW where
    index reg = fromIntegral $ fromEnum reg
instance Indexable GPRegisterB where
    index reg = fromIntegral $ fromEnum reg
instance Indexable SegRegister where
    index reg = fromIntegral $ fromEnum reg

instance Indexable Register where
    index r =
       case r of 
         RegL reg -> index reg
         RegW reg -> index reg
         RegB reg -> index reg
         SReg sr  -> index sr

{-
 - Machine operations
 -}
type Symbol = String

data Instr = OpPush | OpRet | OpLRet | OpInt | OpAdd | OpMov | OpJmp
           | OpCmp  | OpJe  | OpJne  | OpJa  | OpJna | OpJnae| OpJge 
           | OpJae  | OpJl  | OpJle  | OpJg  | OpJnp | OpJp  | OpJno  
           | OpJo   | OpJs  | OpJns  | OpJc  | OpJnc | OpJbe | OpJecxz
           | OpIMul
  deriving (Show, Read, Eq, Enum)

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
