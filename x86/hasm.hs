{- 
 -  This is strictly 32-bit mode x86 assembler.
 -}

import Data.Word
import Data.Int
import Data.Bits
import Data.Char
import Data.List
import Data.Maybe (fromJust)
import Data.Binary.Put (putWord32le, putWord16le, runPut)
import Data.ByteString.Lazy.Internal (unpackBytes)
import qualified Data.ByteString as B
import Text.Printf (printf)
import System.IO (hFlush, stdout)

int :: (Integral a, Num b) => a -> b
int = fromIntegral

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

data Directive = DrctvText | DrctvData | DrctvAsciz String | DrctvAscii String
               | DrctvAlign Int | DrctvFile String 

data Label = Label String

data OpPrefix 
    = PreLock | PreREP | PreREPE | PreREPZ | PreREPNE | PreREPNZ
    | PreCS | PreSS | PreDS | PreES | PreFS | PreGS 
    | PreAddrOverride | PreOffsetOverride
  deriving (Show, Read, Eq)

data OpOperand
    = OpndRel8 Int8 | OpndRel32 Int32 | OpndPtr Int16 Int32
    | OpndReg8 GPRegisterB | OpndGPReg16 GPRegisterW | OpndSegReg SegRegister
    | OpndReg GPRegister | OpndImmB Word8 | OpndImmW Word16 | OpndImmL Word32
    | OpndMem8 Int8 | OpndRM SIB Displacement | OpndZero
 deriving (Show, Read)


data Displacement = NoDispl | Displ8 Int8 | Displ32 Int32 deriving (Show, Read)
data SIB = SIB { 
    sibScale :: Word8, 
    sibIndex :: Maybe GPRegister, 
    base :: Maybe GPRegister }
  deriving (Show, Read)

makeSIB ::  {-Scaling:-}Word8 -> GPRegister -> Maybe GPRegister -> Word8
makeSIB sc ind reg = 
   case lookup sc (zip [1,2,4,8] [0, 1, 2, 3]) of
     Nothing -> error "scaling must be one of [1,2,4,8]"
     Just scbits -> (scbits `shiftL` 6) .|. (indbits `shiftL` 3) .|. basebits
  where basebits = maybe 5 index reg
        indbits = fromJust $ lookup ind (zip reglist [0,1,2,3,5,6,7])
        reglist = [RegEAX, RegECX, RegEDX, RegEBX, RegEBP, RegESI, RegEDI]

              
data Operation 
    = OpPush OpOperand
    | OpRet (Maybe Word16)
    | OpLRet (Maybe Word16)
    | OpAdd OpOperand OpOperand
  deriving (Show, Read)

data HAsmStmt = HAsmDirective Directive
              | HAsmOperation (Maybe OpPrefix) Operation
              | HAsmLabel Label

type HAsmSource = [HAsmStmt]


class Indexable a where
    index :: a -> Word8
    
instance Indexable GPRegister where
    index reg = case lookup reg (zip allGPRegs [0..7]) of
                    Just ind -> ind
instance Indexable GPRegisterW where
    index reg = case lookup reg (zip allGPWRegs [0..7]) of
                    Just ind -> ind
instance Indexable GPRegisterB where
    index reg = case lookup reg (zip allGPBRegs [0..7]) of
                    Just ind -> ind
                    Nothing -> error "SPL/BPL/SIL/DIL registers can't be indexed"
instance Indexable SegRegister where
    index reg = case lookup reg (zip allSegRegs [0..5]) of  
                    Just ind -> ind


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

instance Serializable Word8 where
    bytecode imm8 = [imm8] 
instance Serializable Word16 where
    bytecode = unpackBytes . runPut . putWord16le
instance Serializable Word32 where
    bytecode = unpackBytes . runPut . putWord32le

instance Serializable Int8 where
    bytecode imm8 = [fromIntegral imm8]
instance Serializable Int32 where
    bytecode = unpackBytes . runPut . putWord32le . fromIntegral

-- PUSH
bytesPush :: OpOperand -> [Word8]
bytesPush (OpndSegReg sReg) = 
    case lookup sReg opcodes of
      Just bytes -> bytes                            
    where opcodes = [(RegES, [0x06]), (RegCS, [0x0e]), (RegSS, [0x16]),
                     (RegDS, [0x1e]), (RegFS, [0x0f, 0xa0]), (RegGS, [0x0f, 0xa8])]
bytesPush (OpndImmB imm8) = [0x6a, imm8]
bytesPush (OpndImmL imm) = (0x68 : bytecode imm)
bytesPush (OpndReg reg) = [0x50 + int (index reg)]
{--bytesPush (OpndRM sib Nothing) =
    case base sib of
      Just reg ->
        case sibIndex sib of
--}

-- RET, LRET
bytesRet, bytesLRet :: (Maybe Word16) -> [Word8]
bytesRet (Just imm16) = (0xc2 : bytecode imm16)
bytesRet _            = [0xc3]
bytesLRet (Just imm16) = (0xca : bytecode imm16)
bytesLRet _            = [0xcb]

-- ADD
bytesAdd :: OpOperand -> OpOperand -> [Word8]
bytesAdd (OpndImmB imm) (OpndReg8 reg) | reg == RegAL  = [0x04, imm]
bytesAdd (OpndImmL imm) (OpndReg reg) | reg == RegEAX  = (0x05 : bytecode imm)
bytesAdd (OpndImmB imm) (OpndRM sib displ) = [0x80] ++ (modRM:byteSIB) ++ disp ++ [imm]
  where modRM = (mod `shiftL` 6) .|. bitsRM
        (mod, disp) = case displ of
                        NoDispl -> (0, [])
                        Displ8 dspl -> (1, bytecode dspl)
                        Displ32 dspl -> (2, bytecode dspl)
        (bitsRM, byteSIB) = case sib of
            (SIB sc (Just ind) (Just reg)) -> (4, [makeSIB sc ind (Just reg)])
            (SIB _ Nothing (Just reg)) ->
                case reg of
                  RegEAX -> (0, [])
                  RegECX -> (1, [])
                  RegEDX -> (2, [])
                  RegEBX -> (3, [])
                  RegESI -> (6, [])
                  RegEDI -> (7, [])
                  RegESP -> (4, [0x24])
                  RegEBP -> error "%ebp can't be SIB base" 
            (SIB sc (Just ind) Nothing) ->
                if mod == 0 then (4, [ makeSIB sc ind Nothing ])
                            else error "aaa"

bytesAdd (OpndImmB imm) (OpndReg8 reg) = [0x80, modRM, imm]
   where modRM = 0xC0 .|. bitsRM 
         bitsRM = case lookup reg (zip allGPBRegs [0..7]) of 
                    Just bits -> bits

bytesAdd (OpndImmL imm) (OpndReg reg) = (0x81 : modRM : bytecode imm)
   where modRM = 0xC0 .|. index reg

instance Serializable Operation where
    bytecode (OpPush op) = bytesPush op
    bytecode (OpRet mbClear) = bytesRet mbClear
    bytecode (OpLRet mbClear) = bytesLRet mbClear
    bytecode (OpAdd op1 op2) = bytesAdd op1 op2

main = do
    putStr "**HASM**> " >> hFlush stdout
    op <- readLn :: IO Operation
    mapM_ (putStr . printf "%02x ") $ bytecode op 
    putStrLn ""
    main
