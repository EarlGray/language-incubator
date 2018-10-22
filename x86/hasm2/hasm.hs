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


type InstructionSyntax = (String, [OpArgument -> Bool])

type EncodingError = String

data CodeByArgSize
    = ForAnySize [Word8]
    | ForSize8 [Word8]
    | ForSize32 [Word8]
    | ForSize32Reg [Word8]              -- [op + index(%reg)]
    | ForSize8and32 [Word8] [Word8]

data Encoder = Encoder {
    getEncoder :: [OpArgument] -> Either EncodingError [Word8],
    getInstruction :: CodeByArgSize
}

modRMmd0  r rm  = 0x00 .|. (r `shiftL` 3) .|. (regIndex rm)
modRMmd8  r rm  = 0x40 .|. (r `shiftL` 3) .|. (regIndex rm)
modRMmd32 r rm  = 0x80 .|. (r `shiftL` 3) .|. (regIndex rm)
modRMreg  r rm  = 0xc0 .|. (r `shiftL` 3) .|. (regIndex rm)

encodeMemRef :: Word8 -> (Displacement, Maybe NamedReg32, Maybe (SIBScale, NamedReg32))
                -> Either EncodingError [Word8]
encodeMemRef reg (d, Nothing, Nothing) =
    -- `displ(%ebp)` means `displ`:
    let modRM = modRMmd0 reg (Reg32 EBP) in
    Right $ [modRM] ++ encodeDisplacement d
encodeMemRef reg (d, (Just base), Nothing) =
    -- because %esp in modrm means "use sib":
    let espSIB = if base == ESP then [0x24] else []
    in case (base, d) of
      (EBP, NoDispl) ->
        -- because (%ebp) must be 0(%ebp):
        encodeMemRef reg (Displ8 0, Just base, Nothing)
      (_, NoDispl) ->
        Right (modRMmd0 reg (Reg32 base) : espSIB)
      (_, Displ8 d) ->
        Right ([modRMmd8 reg (Reg32 base)] ++ espSIB ++ [d])
      (_, Displ32 d) ->
        Right ([modRMmd32 reg (Reg32 base)] ++ espSIB ++ dbytes)
          where dbytes = BL.unpack $ Bin.runPut $ Bin.putWord32le d
encodeMemRef reg (d, Nothing, Just (index, sc)) =
    Left "TODO"
encodeMemRef reg (d, Just base, Just (index, sc)) =
    Left "TODO"

encodeImm :: Imm -> [Word8]
encodeImm imm = BL.unpack $ Bin.runPut $ case imm of
    Imm8 imm -> Bin.putWord8 imm
    Imm16 imm -> Bin.putWord16le imm
    Imm32 imm -> Bin.putWord32le imm

encodeDisplacement (NoDispl) = []
encodeDisplacement (Displ8 d)  = [d]
encodeDisplacement (Displ32 d) =
    BL.unpack $ Bin.runPut $ Bin.putWord32le d



encNP _ = Right []

encI8 [ArgImm (Imm8 val)] = Right [val]
encI8 args = Left $ "expected imm8, got: " ++ show args

-- First Displacement?
encFD [ArgMem (MemRef d _ _), _] = Right $ encodeDisplacement d
encFD args = Left $ "Expected `op displ(_), _`, got: " ++ show args

encTD [_, ArgMem (MemRef d _ _)] = Right $ encodeDisplacement d
encTD args = Left $ "Expected `op _, displ(_)`, got: " ++ show args


encMR [ArgReg reg, ArgReg rm] =
    Right [modRMreg (regIndex reg) rm]
encMR [ArgReg reg, ArgMem (MemRef d base index)] =
    encodeMemRef (regIndex reg) (d, base, index)
encMR args = Left $ "expected `op %reg, r/m`, got: " ++ show args

encM stub [ArgMem (MemRef displ base index)] =
    encodeMemRef stub (displ, base, index)
encM _ args = Left $ "expected `op r/m`, got: " ++ show args

encRM [arg1@(ArgMem _), arg2@(ArgReg _)] = encMR [arg2, arg1]
encRM args = Left $ "expected `op r/m, %reg`, got: " ++ show args

encMI stub [ArgReg r, ArgImm imm] =
    Right (modRMreg stub r : encodeImm imm)
encMI stub args =
    Left $ "expected `op/" ++ show stub ++ " %reg, $imm`, got: " ++ show args

encO [ArgReg r] = Right []
encO args = Left $ "expected %reg, got: " ++ show args

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

opMoff (ArgMem (MemRef _ Nothing Nothing)) = True
opMoff _ = False

opMem (ArgMem _) = True
opMem _ = False

opRM arg = opReg arg || opMem arg


--- instruction encodings:

instructions :: [(InstructionSyntax, Encoder)]
instructions = [
  (("int", [opLit 3]),      Encoder encNP (ForSize8 [0xCC])),
  (("int", [opImm8]),       Encoder encI8 (ForSize8 [0xCD])),
  (("into", []),            Encoder encNP (ForAnySize [0xCE])),

  --(("mov", [opImm, opReg]),  ForSize8and32 [0xb0] [0xb8],    Encoder encoderOI),
  (("mov", [opReg, opImm]),     Encoder (encMI 0) (ForSize8and32 [0xc6] [0xc7])),
  (("mov", [opMoff, opRegA]),   Encoder encFD   (ForSize8and32 [0xa0] [0xa1])),
  (("mov", [opRegA, opMoff]),   Encoder encTD   (ForSize8and32 [0xa2] [0xa3])),
  (("mov", [opReg, opRM]),      Encoder encMR   (ForSize8and32 [0x88] [0x89])),
  (("mov", [opRM, opReg]),      Encoder encRM   (ForSize8and32 [0x8a] [0x8b])),
  -- (("mov", [opSReg, opReg16]), ForSize16 [0x8c],     EncoderRM),
  -- (("mov", [opReg16, opSReg]), ForSize16 [0x8e],     EncoderRM),

  (("push", [opReg]),        Encoder encO   (ForSize32Reg [0x50])),
  (("push", [opRM]),         Encoder (encM 6) (ForSize32 [0xff]))
  ]


-- |
-- | Assembling
-- |

findEncoders :: (String, [OpArgument]) -> Either EncodingError Encoder
findEncoders (name, args) = maybe no_enc found $ L.find by_name_args instructions
  where
    no_enc = Left $ "could not find encoder for " ++ name ++ " " ++ show args
    by_name_args ((instr, argspec), _) =
        name == instr && all id (zipWith ($) argspec args)
    found (_, enc) = Right enc

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
        ForSize32Reg [b] ->
            let [ArgReg r] = args
            in Right [b + regIndex r]
        _ -> Left $ "no encoding for " ++ show args

encodeInstruction :: Instruction -> Either EncodingError [Word8]
encodeInstruction (Op prefs name args) = do
    Encoder arg_enc op_enc <- findEncoders (name, args)
    let pref_bits = concat <$> sequenceA (map encodePrefix prefs)
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
esp = ArgReg $ Reg32 ESP

moff d          = ArgMem $ MemRef (Displ32 d) Nothing Nothing
moff8 d         = ArgMem $ MemRef (Displ8 d)  Nothing Nothing
ref1 r          = ArgMem $ MemRef NoDispl     (Just r) Nothing
ref8 d r        = ArgMem $ MemRef (Displ8 d) (Just r) Nothing
refd d r        = ArgMem $ MemRef (Displ32 d) (Just r) Nothing
ref3 (b, i, s)  = ArgMem $ MemRef NoDispl     (Just b) (Just (s, i))
ref d (b, i, s) = ArgMem $ MemRef (Displ32 d) (Just b) (Just (s, i))

mov = Op [] "mov"
movTests = [
  (mov [eax, eax],                     Right [0x89, 0xc0]),
  (mov [ref1 EAX, eax],                Right [0x8b, 0x00]),
  (mov [ref1 ESP, eax],                Right [0x8b, 0x04, 0x24]),
  (mov [ref1 EBP, eax],                Right [0x8b, 0x45, 0x00]),
  (mov [ref8 4(ECX), eax],             Right [0x8b, 0x41, 0x04]),
  (mov [ref8 4(EBP), eax],             Right [0x8b, 0x45, 0x04]),
  (mov [ref8 4(ESP), eax],             Right [0x8b, 0x44, 0x24, 0x04]),
  (mov [moff 0xdeadbeef, eax],         Right [0xa1, 0xef, 0xbe, 0xad, 0xde]),
  (mov [moff 0xdeadbeef, ecx],         Right [0x8b, 0x0d, 0xef, 0xbe, 0xad, 0xde])
  ]

push = Op [] "push"
pushTests = [
  (push [eax],                         Right [0x50]),
  (push [esp],                         Right [0x54])
  ]

runTests tests = do
    let errors = testCodegen tests
    putStrLn $ printf "%d tests passed" (length tests - length errors)
    putStrLn ""
    forM_ errors $ \(op, expected, got) -> do
        putStrLn $ "For: " ++ pretty op
        putStrLn $ "  expected: " ++ show expected
        putStrLn $ "       got: " ++ show got
        putStrLn ""

main = do
    runTests movTests
    runTests pushTests
