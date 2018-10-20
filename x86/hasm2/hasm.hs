import Data.Word
import Data.Bits
import Data.Maybe (mapMaybe)
import qualified Data.List as L

import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.Put as Bin

import Control.Monad (forM_)

import Text.Printf (printf)

int :: (Num b, Integral a) => a -> b
int = fromIntegral


data Imm = Imm8 Word8 | Imm16 Word16 | Imm32 Word32
    deriving (Show, Eq)

immValue :: Imm -> Word32
immValue (Imm8 i8) = int i8
immValue (Imm16 i16) = int i16
immValue (Imm32 i32) = i32

-- | 
-- | Registers and memory references
-- |

data NamedReg8 = AL | CL | DL | BL | AH | CH | DH | BH
    deriving (Enum, Eq, Show)
data NamedReg16 = AX | CX | DX | BX | SP | BP | SI | DI
    deriving (Enum, Eq, Show)
data NamedReg32 = EAX | ECX | EDX | EBX | ESP | EBP | ESI | EDI
    deriving (Enum, Eq, Show)
data NamedSReg = ES | CS | SS | DS | FS | GS
    deriving (Enum, Eq, Show)

data NamedReg = Reg8 NamedReg8 | Reg16 NamedReg16 | Reg32 NamedReg32
    deriving (Show, Eq)

regIndex :: NamedReg -> Word8
regIndex (Reg8 r)   = int $ fromEnum r
regIndex (Reg16 r)  = int $ fromEnum r
regIndex (Reg32 r)  = int $ fromEnum r


data Displacement = NoDispl | Displ8 Word8 | Displ32 Word32
    deriving (Eq, Show)
data SIBScale = ScaleOne | ScaleTwo | ScaleFour | ScaleEight
    deriving (Enum, Eq, Show)

data MemRef = MemRef {
    refDispl  :: Displacement,
    refBase   :: Maybe NamedReg32,
    refOffset :: Maybe (SIBScale, NamedReg32)
} deriving (Show)

displacement :: Displacement -> Word32
displacement NoDispl = 0
displacement (Displ8 d) = int d
displacement (Displ32 d) = d

-- | 
-- | Instructions
-- |

data OpArgument
    = ArgImm Imm
    | ArgReg NamedReg
    | ArgMem MemRef
    deriving (Show)

data Instruction = Op {
    opPrefix :: [String],
    opName :: String,
    opArgs :: [OpArgument]
} deriving (Show)

data ArgSize = S8 | S16 | S32 deriving (Show)

isSize8 :: OpArgument -> Bool
isSize8 (ArgImm (Imm8 _)) = True
isSize8 (ArgReg (Reg8 _)) = True
isSize8 (ArgMem _) = True
isSize8 _ = False

isSize32 :: OpArgument -> Bool
isSize32 (ArgReg (Reg32 _)) = True
isSize32 (ArgImm (Imm32 _)) = True
isSize32 (ArgMem _) = True
isSize32 _ = False


-- |
-- | Instructions pretty-printing
-- |

class PrettyPrintable a where
    pretty :: a -> String

instance PrettyPrintable NamedReg8 where
    pretty r = '%' : (names !! fromEnum r)
      where names = ["al", "cl", "dl", "bl", "ah", "ch", "dh", "bh"]

instance PrettyPrintable NamedReg16 where
    pretty r = '%' : (names !! fromEnum r)
      where names = ["ax", "cx", "dx", "bx", "sp", "bp", "si", "di"]

instance PrettyPrintable NamedReg32 where
    pretty r = '%' : 'e' : (names !! fromEnum r)
      where names = ["ax", "cx", "dx", "bx", "sp", "bp", "si", "di"]

instance PrettyPrintable NamedReg where
    pretty (Reg8 r) = pretty r
    pretty (Reg16 r) = pretty r
    pretty (Reg32 r) = pretty r

instance PrettyPrintable Imm where
    pretty = printf "$0x%x" . immValue

instance PrettyPrintable SIBScale where
    pretty s = ["1248" !! fromEnum s]

instance PrettyPrintable Displacement where
    pretty NoDispl = ""
    pretty d = printf "0x%x" (displacement d)


instance PrettyPrintable MemRef where
    pretty (MemRef d Nothing Nothing) = pretty d
    pretty (MemRef d mbase moffset) = concat [pd, "(", pb, po, ")"]
      where
        pd = pretty d
        pb = maybe "" pretty mbase
        po = maybe "" (\(i, s) -> ", " ++ pretty i ++ ", " ++ pretty s) moffset

instance PrettyPrintable OpArgument where
    pretty (ArgImm imm) = pretty imm
    pretty (ArgReg reg) = pretty reg
    pretty (ArgMem ref) = pretty ref

instance PrettyPrintable Instruction where
    pretty (Op prefs name args) = concat [
        if not (null prefs) then L.intercalate " " prefs ++ " "  else "" ,
        name, " ",
        L.intercalate ", " (map pretty args)
        ]


-- | 
-- | Encoding of instructions
-- | 

type RegMask = Word8
type ImmMask = Word32

newtype RegMask8  = RegMask8  { maskReg8  :: RegMask }
newtype RegMask16 = RegMask16 { maskReg16 :: RegMask }
newtype RegMask32 = RegMask32 { maskReg32 :: RegMask }

-- | ModeCompatibility

data CPUFeature = None | SSE

data ModeCompatibility = ModeCompatibility {
    compat32 :: Bool,
    compat64 :: Bool,
    compatFeature :: CPUFeature
}

mode_all = ModeCompatibility True True None



preAddrsz = 0x67
preLock = 0xF0
prefixes = [
  ("lock",      [preLock]),
  ("repne",     [0xF2]),
  ("repnz",     [0xF2]),
  ("likely",    [0x2E]),
  ("unlikely",  [0x3E])
  ]

encodePrefix :: String -> Either EncodingError [Word8]
encodePrefix pre = maybe err Right $ lookup pre prefixes
   where err = Left $ "prefix is unknown: " ++ pre


type EncodingError = String
data Encoder = Encoder {
    getEncoder :: [OpArgument] -> Either EncodingError [Word8]
}

encodeImm :: Imm -> [Word8]
encodeImm imm = BL.unpack $ Bin.runPut $ case imm of
    Imm8 imm -> Bin.putWord8 imm
    Imm16 imm -> Bin.putWord16le imm
    Imm32 imm -> Bin.putWord32le imm

encodeDisplacement (NoDispl) = []
encodeDisplacement (Displ8 d)  = [d]
encodeDisplacement (Displ32 d) =
    BL.unpack $ Bin.runPut $ Bin.putWord32le d


modRMmd0  r rm  = 0x00 .|. (regIndex r `shiftL` 3) .|. (regIndex rm)
modRMmd8  r rm  = 0x40 .|. (regIndex r `shiftL` 3) .|. (regIndex rm)
modRMmd32 r rm  = 0x80 .|. (regIndex r `shiftL` 3) .|. (regIndex rm)
modRMreg  r rm  = 0xc0 .|. (regIndex r `shiftL` 3) .|. (regIndex rm)

encodeNP _ = Right []

encodeI8 [ArgImm (Imm8 val)] = Right [val]
encodeI8 args = Left $ "expected imm8, got: " ++ show args

-- First Displacement?
encodeFD [ArgMem (MemRef d _ _), _] = Right $ encodeDisplacement d
encodeFD args = Left $ "Expected `op displ(_), _`, got: " ++ show args

encodeTD [_, ArgMem (MemRef d _ _)] = Right $ encodeDisplacement d
encodeTD args = Left $ "Expected `op _, displ(_)`, got: " ++ show args


encodeMR [ArgReg reg, ArgReg rm] = 
    Right [modRMreg reg rm]
encodeMR [ArgReg reg, ArgMem (MemRef d Nothing Nothing)] =
    -- `displ(%ebp)` means `displ`:
    Right $ [modRMmd0 reg (Reg32 EBP)] ++ encodeDisplacement d
encodeMR [ArgReg reg, ArgMem (MemRef d (Just base) Nothing)] =
    -- because %esp in modrm means "use sib":
    let espSIB = if base == ESP then [0x24] else []
    in case (base, d) of
      (EBP, NoDispl) ->
        -- because (%ebp) must be 0(%ebp):
        encodeMR [ArgReg reg, ArgMem (MemRef (Displ8 0) (Just base) Nothing)]
      (_, NoDispl) ->
        Right (modRMmd0 reg (Reg32 base) : espSIB)
      (_, Displ8 d) ->
        Right ([modRMmd8 reg (Reg32 base)] ++ espSIB ++ [d])
      (_, Displ32 d) ->
        Right ([modRMmd32 reg (Reg32 base)] ++ espSIB ++ dbytes)
          where dbytes = BL.unpack $ Bin.runPut $ Bin.putWord32le d
encodeMR [ArgReg reg, ArgMem (MemRef d Nothing (Just (index, sc)))] =
    Left "TODO"
encodeMR [ArgReg reg, ArgMem (MemRef d (Just base) (Just (index, sc)))] =
    Left "TODO"
encodeMR args = Left $ "expected `op %reg, r/m`, got: " ++ show args

encodeRM [arg1@(ArgMem _), arg2@(ArgReg _)] = encodeMR [arg2, arg1]
encodeRM args = Left $ "expected `op r/m, %reg`, got: " ++ show args

encoderMI stub [ArgReg r, ArgImm imm] =
    Right (modRMreg (Reg32 (toEnum stub)) r : encodeImm imm)
encoderMI stub args = 
    Left $ "expected `op/" ++ show stub ++ " %reg, $imm`, got: " ++ show args

data CodeByArgSize
    = ForAnySize [Word8]
    | ForSize8 [Word8]
    | ForSize8and32 [Word8] [Word8]

type InstructionSyntax = (String, [OpArgument -> Bool])

--- operand predicates:
opLit val (ArgImm imm) = val == immValue imm
opLit _ _ = False

opImm (ArgImm _) = True
opImm _ = False

opImm8 (ArgImm (Imm8 _)) = True
opImm8 _ = False

opReg (ArgReg _) = True
opReg _ = False

opRegA (ArgReg r) = regIndex r == 0
opRegA _ = False

opRegFor index (ArgReg r) = index == regIndex r
opRegFor _ _ = False

opRegExcept index (ArgReg r) = index /= regIndex r
opRegExcept _ _ = False

opMoff (ArgMem (MemRef _ Nothing Nothing)) = True
opMoff _ = False

opMem (ArgMem _) = True
opMem _ = False

opRM arg = opReg arg || opMem arg


--- instruction encodings:

instructions :: [(InstructionSyntax, CodeByArgSize, Encoder)]
instructions = [
  (("int", [opLit 3]),      ForSize8 [0xCC],             Encoder encodeNP),
  (("int", [opImm8]),       ForSize8 [0xCD],             Encoder encodeI8),
  (("into", []),            ForAnySize [0xCE],           Encoder encodeNP),

  --(("mov", [opImm, opReg]),  ForSize8and32 [0xb0] [0xb8],    Encoder encoderOI),
  (("mov", [opReg, opImm]),  ForSize8and32 [0xc6] [0xc7],    Encoder (encoderMI 0)),
  (("mov", [opMoff, opRegA]), ForSize8and32 [0xa0] [0xa1],   Encoder encodeFD),
  (("mov", [opRegA, opMoff]), ForSize8and32 [0xa2] [0xa3],   Encoder encodeTD),
  (("mov", [opReg, opRM]),  ForSize8and32 [0x88] [0x89],  Encoder encodeMR),
  (("mov", [opRM, opReg]),  ForSize8and32 [0x8a] [0x8b],  Encoder encodeRM)
  -- (("mov", [opSReg, opReg16]), ForSize16 [0x8c],     EncoderRM),
  -- (("mov", [opReg16, opSReg]), ForSize16 [0x8e],     EncoderRM),
  ]

{-
  (("xor", [opA8,  OpImm S8]), mode_all,  [0x34],           EncodeI),
  (("xor", [opADef, OpImm SDef]), mode_all, [0x35],         EncodeI),
  (("xor", [opRM8, OpImm S8]),  mode_all,   [0x80],         EncodeMI 6),
  (("xor", [opRMa, OpImm SAlt]), mode_all,  [preAddrsz, 0x81],  EncodeMI 6),
  (("xor", [opRM,  OpImm SDef]), mode_all,  [0x81],         EncodeMI 6),
  (("xor", [opRM8, opR8]),      mode_all,   [0x30],         EncodeMR),
  (("xor", [opRM,  opR]),        mode_all,   [0x31],         EncodeMR)
  -}


-- | 
-- | Assembling
-- |

findEncoders :: (String, [OpArgument]) -> Either EncodingError (CodeByArgSize, Encoder)
findEncoders (name, args) = maybe no_enc found $ L.find by_name_args instructions
  where
    no_enc = Left $ "could not find encoder for " ++ name ++ " " ++ show args
    by_name_args ((instr, argspec), _, _) = 
        name == instr && all id (zipWith ($) argspec args)
    found (_, op_enc, arg_enc) = Right (op_enc, arg_enc)

encodeOp codeforsize args = 
    case codeforsize of
        ForAnySize bytes ->
            Right bytes
        ForSize8 bytes | all isSize8 args -> 
            Right bytes
        ForSize8and32 bytes _ | all isSize8 args ->
            Right bytes
        ForSize8and32 _ bytes | all isSize32 args ->
            Right bytes
        _ -> Left $ "no encoder for " ++ show args

encodeInstruction :: Instruction -> Either EncodingError [Word8]
encodeInstruction (Op prefs name args) = do
    (op_enc, Encoder arg_enc) <- findEncoders (name, args)
    let pref_bits = concat <$> sequenceA (map encodePrefix prefs) :: Either EncodingError [Word8]
    let op_bits = encodeOp op_enc args
    let args_bits = arg_enc args :: Either EncodingError [Word8]
    concat <$> sequenceA [pref_bits, op_bits, args_bits]
    

{-
 -  Tests
 -}
testCodegen tests = mapMaybe test tests
  where
    test (op, expected) = 
      let got = encodeInstruction op 
      in if got == expected
        then Nothing
        else Just $ (op, expected, got)

imm n = ArgImm (Imm32 n)

eax = ArgReg $ Reg32 EAX
ecx = ArgReg $ Reg32 ECX

moff d          = ArgMem $ MemRef (Displ32 d) Nothing Nothing 
moff8 d         = ArgMem $ MemRef (Displ8 d)  Nothing Nothing 
ref1 r          = ArgMem $ MemRef NoDispl     (Just r) Nothing
ref8 d r        = ArgMem $ MemRef (Displ8 d) (Just r) Nothing 
refd d r        = ArgMem $ MemRef (Displ32 d) (Just r) Nothing 
ref3 (b, i, s)  = ArgMem $ MemRef NoDispl     (Just b) (Just (s, i))
ref d (b, i, s) = ArgMem $ MemRef (Displ32 d) (Just b) (Just (s, i))

movTests = [
  (Op [] "mov" [eax, eax],                     Right [0x89, 0xc0]),
  (Op [] "mov" [ref1 EAX, eax],                Right [0x8b, 0x00]),
  (Op [] "mov" [ref1 ESP, eax],                Right [0x8b, 0x04, 0x24]),
  (Op [] "mov" [ref1 EBP, eax],                Right [0x8b, 0x45, 0x00]),
  (Op [] "mov" [ref8 4(ECX), eax],             Right [0x8b, 0x41, 0x04]),
  (Op [] "mov" [ref8 4(EBP), eax],             Right [0x8b, 0x45, 0x04]),
  (Op [] "mov" [ref8 4(ESP), eax],             Right [0x8b, 0x44, 0x24, 0x04]),
  (Op [] "mov" [moff 0xdeadbeef, eax],         Right [0xa1, 0xef, 0xbe, 0xad, 0xde]),
  (Op [] "mov" [moff 0xdeadbeef, ecx],         Right [0x8b, 0x0d, 0xef, 0xbe, 0xad, 0xde])
  ]

main = do
    let errors = testCodegen movTests
    putStrLn $ printf "%d tests passed" (length movTests - length errors)
    putStrLn ""
    forM_ errors $ \(op, expected, got) -> do
        putStrLn $ "For: " ++ pretty op
        putStrLn $ "  expected: " ++ show expected
        putStrLn $ "       got: " ++ show got
        putStrLn ""
