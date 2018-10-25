import Data.Word
import Data.Int
import Data.Bits
import Data.Maybe (mapMaybe)
import qualified Data.Either as Ei
import qualified Data.Map as M
import qualified Data.List as L

import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.Put as Bin

import Control.Monad (forM_)

import Text.Printf (printf)

int :: (Num b, Integral a) => a -> b
int = fromIntegral


-- |
-- | Immediate values, as encoded after size/spec check
-- |

data Imm = Imm8 Word8 | Imm16 Word16 | Imm32 Word32
    deriving (Show, Eq)

immValue :: Imm -> Word32
immValue (Imm8 i8) = int i8
immValue (Imm16 i16) = int i16
immValue (Imm32 i32) = i32

immCast8s (Imm8 v) | v < 0x80 = Right (Imm8 v)
immCast8s (Imm16 v) =
    let sv = (int v :: Int16)
    in if -128 <= sv && sv < 128
        then Right (Imm8 (int (int sv :: Word16)))
        else Left $ "cannot cast signed number to imm8: " ++ show v
immCast8s (Imm32 v) =
    let sv = (int v :: Int32)
    in if -128 <= sv && sv < 128
        then Right (Imm8 (int (int sv :: Word32)))
        else Left $ "cannot cast signed number to imm8: " ++ show v


-- |
-- | Registers and memory references
-- | the same in input and spec
-- |

data NamedReg8 = AL | CL | DL | BL | AH | CH | DH | BH
    deriving (Enum, Eq, Show)
data NamedReg16 = AX | CX | DX | BX | SP | BP | SI | DI
    deriving (Enum, Eq, Show)
data NamedReg32 = EAX | ECX | EDX | EBX | ESP | EBP | ESI | EDI
    deriving (Enum, Eq, Show)
data NamedSReg = ES | CS | SS | DS | FS | GS
    deriving (Enum, Eq, Show)

data NamedReg
    = Reg8 NamedReg8
    | Reg16 NamedReg16
    | Reg32 NamedReg32
    | SReg NamedSReg
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
-- | Instructions as input
-- |

data OpArgument
    = ArgImm Word32
    | ArgReg NamedReg
    | ArgMem MemRef
    deriving (Show)

data Instruction = Op {
    opPrefix :: [String],
    opName :: String,
    opArgs :: [OpArgument]
} deriving (Show)

-- after size check:
data Arg
    = AImm Imm
    | AReg NamedReg
    | AMem ArgSize MemRef
    deriving (Show)

-- |
-- | Instruction specifications
-- |

data ArgSize = S8 | S16 | S32 deriving (Show, Eq)

immSize (Imm8 _) = S8
immSize (Imm16 _) = S16
immSize (Imm32 _) = S32

regSize (Reg8 _) = S8
regSize (Reg16 _) = S16
regSize (Reg32 _) = S32

getSizeOf :: OpArgument -> Maybe ArgSize
getSizeOf (ArgReg reg) = Just $ regSize reg
getSizeOf _ = Nothing

isSizeOf :: OpArgument -> OpArgument -> Bool
isSizeOf (ArgMem _) _ = True
isSizeOf _ (ArgMem _) = True
isSizeOf a b = getSizeOf a == getSizeOf b

isSize8 :: OpArgument -> Bool
isSize8 (ArgImm imm) = imm < 0x100
isSize8 (ArgReg (Reg8 _)) = True
isSize8 (ArgMem _) = True
isSize8 _ = False

isSize16 :: OpArgument -> Bool
isSize16 (ArgImm imm) = imm < 0x10000
isSize16 (ArgReg (Reg16 _)) = True
isSize16 (ArgMem _) = True
isSize16 _ = False

isSize32 :: OpArgument -> Bool
isSize32 (ArgReg (Reg32 _)) = True
isSize32 (ArgImm _) = True
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
    pretty (ArgImm imm) = printf "0x%x" imm
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

type EncodingError = String

type Encoding = Either EncodingError [Word8]

class Encodable a where
    encode :: a -> Encoding

encodeImm :: Imm -> [Word8]
encodeImm imm = BL.unpack $ Bin.runPut $ case imm of
    Imm8 imm -> Bin.putWord8 imm
    Imm16 imm -> Bin.putWord16le imm
    Imm32 imm -> Bin.putWord32le imm

instance Encodable Word32 where
    encode = Right . BL.unpack . Bin.runPut . Bin.putWord32le

instance Encodable Imm where
    encode = Right . encodeImm


encodeDisplacement (NoDispl) = []
encodeDisplacement (Displ8 d)  = [d]
encodeDisplacement (Displ32 d) =
    BL.unpack $ Bin.runPut $ Bin.putWord32le d

instance Encodable Displacement where
    encode = Right . encodeDisplacement


rmmodBaseNoDispl r = 0x00 .|. regIndex r
rmmodBaseDispl8 r  = 0x40 .|. regIndex r
rmmodBaseDispl32 r = 0x80 .|. regIndex r
rmmodDirectReg r   = 0xC0 .|. regIndex r

encodeMemRef :: MemRef -> Encoding
encodeMemRef (MemRef d Nothing Nothing) =
    -- `displ(%ebp)` means `displ`:
    let modRM = rmmodBaseNoDispl (Reg32 EBP) in
    Right $ [modRM] ++ encodeDisplacement d
encodeMemRef (MemRef d (Just base) Nothing) =
    -- because %esp in modrm means "use sib":
    let espSIB = if base == ESP then [0x24] else []
    in case (base, d) of
      (EBP, NoDispl) ->
        -- because (%ebp) must be 0(%ebp):
        encodeMemRef (MemRef (Displ8 0) (Just base) Nothing)
      (_, NoDispl) ->
        Right (rmmodBaseNoDispl (Reg32 base) : espSIB)
      (_, Displ8 d) ->
        Right ([rmmodBaseDispl8 (Reg32 base)] ++ espSIB ++ [d])
      (_, Displ32 d) ->
        Right ([rmmodBaseDispl32 (Reg32 base)] ++ espSIB ++ dbytes)
          where dbytes = BL.unpack $ Bin.runPut $ Bin.putWord32le d
encodeMemRef (MemRef d Nothing (Just (index, sc))) =
    Left "TODO"
encodeMemRef (MemRef d (Just base) (Just (index, sc))) =
    Left "TODO"

withRM :: Word8 -> Encoding -> Encoding
withRM stub enc = do
    (rmmod : bytes) <- enc
    Right $ (stub `shiftL` 3 .&. rmmod : bytes)

withRMreg r = withRM (regIndex r)

instance Encodable MemRef where
    encode = encodeMemRef


encodeRMModReg :: NamedReg -> Encoding
encodeRMModReg r = Right [rmmodDirectReg r]

instance Encodable NamedReg where
    encode = encodeRMModReg


preOpndsz = 0x66
preAddrsz = 0x67

type InstructionSyntax = (String, [OpArgument -> Bool])


data CodeByArgSize
    = ForAnySize [Word8]
    | ForSize8 [Word8]
    | ForSize32 [Word8]                 -- 
    | ForSize32Reg [Word8]              -- [op + index(%reg)]
    | ForSize8and32 [Word8] [Word8]     -- 8, 16 with 0x67, 32
    | ForSize8and32imm [Word8] [Word8]  -- 8, 16 with 0x66, 32

data Encoder = Encoder ([Arg] -> Either EncodingError [Word8]) CodeByArgSize


--- operand predicates:
opLit val (ArgImm imm) = val == imm
opLit _ _ = False

opImm (ArgImm _) = True
opImm _ = False

opImm8 (ArgImm imm) = imm < 0x100
opImm8 _ = False

opImm16 (ArgImm imm) = imm < 0x10000
opImm16 _ = False

-- TODO: sign-extended:
opImm8s (ArgImm imm) = -128 <= v && v < 128
  where v = int imm :: Int32

opReg (ArgReg _) = True
opReg _ = False

opRegA (ArgReg r) = regIndex r == 0
opRegA _ = False

opMoff (ArgMem (MemRef _ Nothing Nothing)) = True
opMoff _ = False

opMem (ArgMem _) = True
opMem _ = False

opRM arg = opReg arg || opMem arg

opRM32 (ArgMem _) = True
opRM32 (ArgReg (Reg16 _)) = True
opRM32 (ArgReg (Reg32 _)) = True
opRM32 _ = False


-- argument encoders:

encNP _ = Right []

encI (AImm imm : _) = encode imm

encMR :: [Arg] -> Encoding
encMR [AReg reg, AMem _ mem] = withRMreg reg $ encodeMemRef mem
encMR args = Left $ "expected `op %reg, r/m`, got: " ++ show args

encM stub [AMem _ mem] = withRM stub $ encodeMemRef mem
encM _ args = Left $ "expected `op r/m`, got: " ++ show args

encRM [m@(AMem _ _), r@(AReg _)] = encMR [r, m] 
encRM args = Left $ "expected `op r/m, %reg`, got: " ++ show args

encMI stub [AReg reg, AImm imm] = do
    (++ encodeImm imm) <$> (withRM stub $ encode reg)
encMI stub [AMem _ mem, AImm imm] =
    (++ encodeImm imm) <$> (withRM stub $ encodeMemRef mem)
encMI stub args =
    Left $ "expected `op/" ++ show stub ++ " %reg, $imm`, got: " ++ show args

encIM stub [imm@(AImm _), reg@(AReg _)] = encMI stub [reg, imm]
encIM stub [imm@(AImm _), mem@(AMem _ _)] = encMI stub [mem, imm]
encIM stub args =
    Left $ "expected `op/" ++ show stub ++ " $imm, %reg`, got: " ++ show args

encIM8s stub [AImm imm, mem@(AMem _ _)] = do
    imm8 <- immCast8s imm
    encIM stub [AImm imm8, mem]

encFD [AReg _, AMem _ (MemRef d Nothing Nothing)] = encode d
encFD args = Left $ "expected _, moff, got: " ++ show args
encTD [r, m] = encFD [m, r]

encO [AReg r] = Right []
encO args = Left $ "expected %reg, got: " ++ show args


--- instruction encodings:

instructions :: [(InstructionSyntax, Encoder)]
instructions = [
  (("add", [opImm8s, opRM32]),  Encoder (encIM8s 0) (ForAnySize [0x83])), -- sign-extended
  (("add", [opImm, opRegA]),    Encoder encI        (ForSize8and32imm [0x04] [0x05])),
  (("add", [opImm, opRM]),      Encoder (encIM 0)   (ForSize8and32imm [0x80] [0x81])),

  (("int", [opLit 3]),          Encoder encNP       (ForSize8 [0xCC])),
  (("int", [opImm8]),           Encoder encI        (ForSize8 [0xCD])),
  (("into",[]),                  Encoder encNP       (ForAnySize [0xCE])),

  (("mov", [opReg, opImm]),     Encoder (encMI 0)   (ForSize8and32 [0xc6] [0xc7])),
  (("mov", [opMoff, opRegA]),   Encoder encFD       (ForSize8and32 [0xa0] [0xa1])),
  (("mov", [opRegA, opMoff]),   Encoder encTD       (ForSize8and32 [0xa2] [0xa3])),
  (("mov", [opReg, opRM]),      Encoder encMR       (ForSize8and32 [0x88] [0x89])),
  (("mov", [opRM, opReg]),      Encoder encRM       (ForSize8and32 [0x8a] [0x8b])),
  -- (("mov", [opSReg, opReg16]), ForSize16 [0x8c],     EncoderRM),
  -- (("mov", [opReg16, opSReg]), ForSize16 [0x8e],     EncoderRM),

  (("push", [opReg]),           Encoder encO        (ForSize32Reg [0x50])),
  (("push", [opRM]),            Encoder (encM 6)    (ForSize32 [0xff])),
  (("push", [opImm]),           Encoder encI        (ForSize8and32imm [0x6a] [0x68])),

  (("ret",  []),            Encoder encNP (ForAnySize [0xc3])),
  (("ret",  [opImm16]),      Encoder encI (ForAnySize [0xc2]))
  ]


-- |
-- | Assembling
-- |
findEncoders :: String -> [OpArgument] -> [Encoder]
findEncoders name args = map snd $ filter by_name_args instructions
  where
    by_name_args ((n, argspec), _) = 
        name == n &&
        length args == length argspec &&
        and (zipWith ($) argspec args)

encodeOp codeforsize args = do
    case codeforsize of
        ForAnySize bytes ->
            Right bytes
        ForSize8 bytes | all isSize8 args ->
            Right bytes
        ForSize8and32 bytes _ | all isSize8 args ->
            Right bytes
        ForSize8and32 _ bytes | all isSize16 args ->
            Right (preAddrsz : bytes)
        ForSize8and32 _ bytes | all isSize32 args ->
            Right bytes
        ForSize8and32imm bytes _ | all isSize8 args ->
            Right bytes
        ForSize8and32imm _ bytes | all isSize16 args ->
            Right (preOpndsz : bytes)
        ForSize8and32imm _ bytes | all isSize32 args ->
            Right bytes
        ForSize32Reg [b] ->
            let [ArgReg r] = args
            in Right [b + regIndex r]
        ForSize32 bytes | all isSize16 args ->  
            Right (preAddrsz : bytes)
        ForSize32 bytes | all isSize32 args ->
            Right bytes
        _ -> Left $ "no encoding for " ++ show args

checkSize :: String -> [OpArgument] -> Either EncodingError [Arg]
checkSize = undefined

encodeInstructionWith :: Encoder -> Instruction -> Encoding
encodeInstructionWith enc op = undefined

encodeInstruction :: Instruction -> Encoding
encodeInstruction op@(Op [] name args) = do
    args' <- checkSize name args
    let encoders = findEncoders name args
    case map (\e -> encodeInstructionWith e op) encoders of
      [] -> Left $ "no encoder found for: " ++ show op
      encs -> case Ei.partitionEithers encs of
        (_, (encoded:_)) -> Right encoded
        ((err:_), _) -> Left err

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

imm n = ArgImm n

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

add = Op [] "add"
addTests = [
  (add [imm 0x42, ArgReg (Reg8 AL)],    Right [0x04, 0x42]),
  (add [imm 0x08, ArgReg (Reg8 DH)],    Right [0x80, 0xc6, 0x08]),
--(add [imm 0xffff, ArgReg (Reg16 AX)], Right [0x66, 0x83, 0xc0, 0xff]), -- gas
  (add [imm 0xffff, ArgReg (Reg16 AX)], Right [0x66, 0x05, 0xff, 0xff]),
--(add [imm 0xffff, ArgReg (Reg16 SP)], Right [0x66, 0x83, 0xc4, 0xff]), -- gas
  (add [imm 0xffff, ArgReg (Reg16 SP)], Right [0x66, 0x81, 0xc4, 0xff, 0xff]),
  (add [imm 0xff, eax],                 Right [0x05, 0xff, 0x00, 0x00, 0x00]),
  (add [imm 0xffff0000, eax],           Right [0x05, 0x00, 0x00, 0xff, 0xff]),
  (add [imm 0xff, ref1 EAX],            Right [0x80, 0x00, 0xff]),
  (add [imm 0xffff, ref1 EAX],        Right [0x66, 0x83, 0x00, 0xff]),
  (add [imm 0xffff, ref1 EAX],          Right [0x81, 0x00, 0xff, 0xff, 0x00, 0x00]),
  (add [imm 0xffffffff, ref1 EAX],      Right [0x83, 0x00, 0xff]),
  (add [imm 1, ref1 EDX],              Right [0x80, 0x02, 0x01]),
  (add [imm 1, ref1 EDX],               Right [0x83, 0x02, 0x01])
  ]

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
  (push [eax],                          Right [0x50]),
  (push [esp],                          Right [0x54]),
  (push [imm 0x42],                     Right [0x6a, 0x42]),
  (push [imm 0xc0fe],                   Right [0x66, 0x68, 0xfe, 0xc0]),
  (push [imm 0xcafebabe],               Right [0x68, 0xbe, 0xba, 0xfe, 0xca])
  ]

retTests = [
  (Op [] "ret" [],                      Right [0xc3]),
  (Op [] "ret" [imm 42],                Right [0xc2, 0x2a, 0x00])
  ]

runTests tests = do
    let errors = testCodegen tests
    putStrLn $ printf "%s\t: %d tests passed" (opName $ fst $ head tests) (length tests - length errors)
    putStrLn ""
    let showhex bytes = concat $ map (printf "%02x ") bytes
    forM_ errors $ \(op, expected, got) -> do
        putStrLn $ "For: " ++ pretty op
        putStrLn $ "  expected: " ++ either id showhex expected
        putStrLn $ "       got: " ++ either id showhex got
        putStrLn ""

main = do
    runTests addTests
    runTests movTests
    runTests pushTests
    runTests retTests
