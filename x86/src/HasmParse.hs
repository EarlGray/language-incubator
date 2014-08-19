module HasmParse where

import Numeric
import Data.Char
import Data.Either (lefts, rights)
import Data.List (intercalate, nub, sort)
import qualified Data.Map as M

import qualified Control.Arrow as Arr
import Control.Applicative ((<$>), (<*), liftA2)
import Control.Monad (when)

import Text.Parsec
import Text.Parsec.String (Parser)

import HasmTypes
import X86CPU

hasmParseFile :: FilePath -> IO (Either ParseError ParseResult)
hasmParseFile fname = hasmParseWithSource fname <$> readFile fname

hasmParseWithSource :: FilePath -> String -> Either ParseError ParseResult
hasmParseWithSource fname ss = parse (asmfile <* eof) fname ss

asmfile :: Parser ParseResult
asmfile = do
  blanklines
  many $ do
    pos <- getPosition
    stmt <- asmstmt
    return (stmt, toHasmPos pos)

asmstmt :: Parser HasmStatement
asmstmt = try asmdir <|> try asmlabel <|> asmop <?> "assembly statement"

blanklines = mbasmspaces >> many ((linecomment <|> newline) >> mbasmspaces)
linecomment = do
  char '#'; many (noneOf "\n"); newline

dirchar = letter <|> char '_'
idschar = letter <|> oneOf "_"
idchar = idschar <|> digit <|> oneOf ".$"

symbol = cons idschar (many idchar)

endstmt = (mbasmspaces >> oneOf ";\n" >> blanklines >> return ()) <|> eof
asmspaces = many1 (oneOf " \t")
mbasmspaces = many (oneOf " \t")

cons a b = liftA2 (:) a b
opchar = letter <|> digit
opcode = cons letter (many1 opchar)
regname = cons letter (many1 opchar)

asmdir = do
  char '.'
  dir <- many1 dirchar
  optional asmspaces
  dirargs <- sepBy (many1 $ noneOf "\n,") (char ',' >> optional asmspaces)
  endstmt
  case readDirective dir dirargs of
    Right d -> return $ HasmStDirective d
    Left e -> parserFail e

asmlabel = HasmStLabel <$> (symbol <* char ':' <* optional blanklines) <?> "label"

asmop = do
  op <- opcode
  mbasmspaces
  opnds <- sepBy asmopnd (char ',' >> mbasmspaces)
  endstmt
  case readOperation op opnds of
    Left e -> parserFail e
    Right o -> return $ HasmStInstr [] o

asmopnd :: Parser OpOperand
asmopnd = try (OpndImm <$> asmimm) <|>
          try (OpndReg <$> asmregister) <|>
          try asmmem <?> "opcode operand"

immToDispl (ImmL imm)
    | (-128 <= imm) && (imm < 128) = Displ8 $ fromIntegral imm
    | otherwise = Displ32 imm

asmdispl = try (immToDispl <$> readIntegerLit)
           <|> (DisplLabel <$> symbol)
           <?> "displacement before ("

asmmem = do
  mbDspl <- optionMaybe asmdispl
  mbSIB <- optionMaybe asmsib
  when (mbDspl == Nothing && mbSIB == Nothing) $ fail "no displacement and no SIB"
  let dspl = maybe NoDispl id mbDspl
  let sib = maybe noSIB id mbSIB
  return $ OpndRM sib dspl


{-- SIBs and memory refs --}
data SIBPart = SIBReg GPRegister | SIBScale Word8 | SIBNothing
immToSIBScale (ImmL imm) = SIBScale $ fromIntegral imm

sibpart = try (SIBReg <$> asmgpregister) <|>
          try (immToSIBScale <$> readIntegerLit) <|>
          return SIBNothing

sibparts = between (char '(') (char ')') $ sepBy sibpart (char ',' >> mbasmspaces)

asmsib = do
  sps <- sibparts
  case sps of
    -- (%reg)
    [SIBReg base] -> return $ SIB 1 Nothing (Just base)
    -- (%reg, %reg)
    [SIBReg base, SIBReg ind] -> return $ SIB 1 (Just ind) (Just base)
    -- (%reg, %reg, )
    [SIBReg base, SIBReg ind, SIBNothing] -> return $ SIB 1 (Just ind) (Just base)
    -- (, %reg)
    [SIBNothing, SIBReg ind] -> return $ SIB 1 (Just ind) Nothing
    -- (%reg, scale)
    [SIBReg ind, SIBScale scale ] -> makeSIB scale (Just ind) Nothing
    -- (, %reg, scale)
    [SIBNothing, SIBReg ind, SIBScale scale ] -> makeSIB scale (Just ind) Nothing
    -- (%reg, %reg, scale)
    [SIBReg base, SIBReg ind, SIBScale scale ] -> makeSIB scale (Just ind) (Just base)
    -- everything else:
    _ -> fail "SIB format is invalid"
  where
    makeSIB scale mbInd mbBase =
      if scale `elem` [1,2,4]
      then return $ SIB (fromIntegral scale) mbInd mbBase
      else fail "Scale is not 1,2,4"


asmregister = do
  char '%'
  reg <- regname
  case mbRegByName reg of
    Just r -> return r
    _ -> parserFail $ "Not a register name: " ++ reg

asmgpregister = do
  RegL reg <- asmregister
  return reg

{-- Number literals --}
asmimm = do
  char '$'
  readIntegerLit

readIntegerLit = (ImmL . fromIntegral) <$> (intneg <|> intparse <?> "number")

intneg = do
    char '-'
    num <- intparse
    return $ negate num

hexint = do
  ds <- many1 (oneOf "0123456789abcdefABCDEF")
  case readHex ds of
    [(num, "")] -> return num
    _ -> parserFail "failed to parse hexadecimal number"

octint = do
  ds <- many1 (oneOf "01234567")
  case readOct ds of
    [(num, "")] -> return num
    _ -> parserFail "failed to parse octal number"

intparse = do
  d <- digit
  if d == '0'
  then do
    try (char 'x' >> hexint) <|> try octint <|> return 0
  else do
    ds <- many digit
    case readDec (d:ds) of
      [(num, "")] -> return num
      _ -> parserFail "failed to parse a number"

{-- Directives --}
readDirective :: String -> [String] -> Either String Directive
readDirective dir args =
  case dir of
    "section" ->
      case args of
        [] -> Left $ ".section takes at least section name"
        [name] -> Right $ DirSection name 0 ""
        [name, subsect] -> case (reads :: ReadS Int) subsect of
                             [(nsubsect, "")] -> Right $ DirSection name nsubsect ""
                             _ -> Left $ "Failed to parse subsection: " ++ show subsect
        _ -> Left $ "invalid .section format"
    sect | sect `elem` ["text", "data"] ->
      case args of
        [] -> Right $ DirSection sect 0 ""
        [subsect] -> case (reads :: ReadS Int) subsect of
                        [(nsubsect, "")] -> Right $ DirSection sect nsubsect ""
                        _ -> Left $ "Failed to parse subsection: " ++ show subsect
    glbl | glbl `elem` ["global", "globl"] ->
      let eiSyms = map (parse (symbol <* eof) "") args
      in case lefts eiSyms of
          [] -> Right $ DirGlobal (rights eiSyms)
          (err:_) -> Left $ "failed to parse symbol " ++ show err
    _ -> Left $ "Unknown directive: " ++ dir ++ " " ++ intercalate "," args


type OpSuffix = Char
type OpInfo = (Instr, [Int], [OpSuffix])

opsyntax :: [(String, OpInfo)]
opsyntax = [
  ("mov",   (OpMov,  [2],   "lwb")),
  ("add",   (OpAdd,  [2],   "lwb")),
  ("ret",   (OpRet,  [0,1], "w"  )),
  ("push",  (OpPush, [1],   "lwb")),
  ("cmp",   (OpCmp,  [2],   "lwb")),
  ("int",   (OpInt,  [1],   "b"  )),
  ("jmp",   (OpJmp,  [1],   "lwb")),
  ("imul",  (OpIMul, [1,2,3],"lwb")),

  ("je",    (OpJe,   [1],   ""   )),  -- ZF=1
  ("jz",    (OpJe,   [1],   ""   )),
  ("jne",   (OpJne,  [1],   ""   )),  -- ZF=0
  ("jnz",   (OpJne,  [1],   ""   )),
  ("jge",   (OpJge,  [1],   ""   )),  -- SF=OF
  ("jnl",   (OpJge,  [1],   ""   )),
  ("jl",    (OpJl,   [1],   ""   )),  -- SF<>OF
  ("jnge",  (OpJl,   [1],   ""   )),
  ("jle",   (OpJle,  [1],   ""   )),  -- ZF=1, SF<>OF
  ("jng",   (OpJle,  [1],   ""   )),
  ("jg",    (OpJg,   [1],   ""   )),  -- ZF=0, SF=OF
  ("jnle",  (OpJg,   [1],   ""   )),
  ("jnp",   (OpJnp,  [1],   ""   )),  -- PF=0
  ("jpo",   (OpJnp,  [1],   ""   )),
  ("jp",    (OpJp,   [1],   ""   )),  -- PF=1
  ("jpe",   (OpJp,   [1],   ""   )),
  ("jno",   (OpJno,  [1],   ""   )),  -- OF=0
  ("jo",    (OpJo,   [1],   ""   )),  -- OF=1
  ("jns",   (OpJns,  [1],   ""   )),  -- SF=0
  ("js",    (OpJs,   [1],   ""   )),  -- SF=1
  
  ("jae",   (OpJnc,  [1],   ""   )),  -- CF=0
  ("jnb",   (OpJnc,  [1],   ""   )),
  ("jnc",   (OpJnc,  [1],   ""   )),
  ("jc",    (OpJc,   [1],   ""   )),  -- CF=1
  ("jb",    (OpJc,   [1],   ""   )),
  ("jnae",  (OpJc,   [1],   ""   )),
  ("ja",    (OpJa,   [1],   ""   )),  -- CF=0, ZF=0
  ("jnbe",  (OpJa,   [1],   ""   )),
  ("jbe",   (OpJbe,  [1],   ""   )),  -- CF=1, ZF=1
  ("jna",   (OpJbe,  [1],   ""   )),

  ("jecxz", (OpJecxz,[1],   ""   ))  ]

opmap = M.fromList opsyntax

oplookup :: String -> Either String (OpInfo, OpSuffix)
oplookup opname =
  case M.lookup opname opmap of
    Just opinfo ->
      Right (opinfo, '?')
    Nothing ->
      -- search without possible suffixes:
      let suf = last opname
          opname' = init opname
      in case M.lookup opname' opmap of
        Just opinfo@(_, _, suffs) ->
          if suf `elem` suffs
          then Right (opinfo, suf)
          else Left $ "Invalid suffix for command " ++ opname'
        Nothing -> Left $ "Unknown instruction: " ++ opname

readOperation :: String -> [OpOperand] -> Either String Operation
readOperation opname opnds =
  case oplookup opname of
    Left e -> Left e
    Right (opinfo@(op, arglens, suffs), suf) ->
      if length opnds `elem` arglens
      then case unifyOperandTypes opinfo suf opnds of
             Right opnds' -> Right $ Operation op opnds'
             Left e -> Left $ "Operand type mismatch: " ++ e
      else Left $ "Opcode '" ++ opname ++ "' does not take " ++ show (length opnds) ++ " parameter(s)"

getOpndType :: OpOperand -> OpSuffix
getOpndType opnd =
  case opnd of
    OpndImm _        -> '?'    -- any, needs adjusting to OpndReg size
    OpndRM _ _       -> '?'    -- memory can be read by 1,2,4 bytes
    OpndReg (SReg _) -> 's'    -- compatible with w and l at the same time
    OpndReg (RegL _) -> 'l'    -- these registers are rigid
    OpndReg (RegW _) -> 'w'
    OpndReg (RegB _) -> 'b'

getOperandsTypes :: OpSuffix -> [OpOperand] -> [OpSuffix]
getOperandsTypes opsuf opnds =
  nub $ sort $ (opsuf : map getOpndType opnds)  -- what types are present?

unifyOperandTypes :: OpInfo -> OpSuffix -> [OpOperand] -> Either String [OpOperand]
unifyOperandTypes (opname, lens, sufs) opsuf opnds =
  let ts = filter (not . flip elem "s?") $ getOperandsTypes opsuf opnds
  in case ts of
      ""    -> Right opnds
      "l"   -> Right opnds  -- parser defaults to ImmL
      "w"   -> adjustImmediatesWith immToW opnds
      "b"   -> adjustImmediatesWith immToB opnds
      _     -> Left $ "Operand type mismatch"  -- TODO: e.g. movsx with different types

adjustImmediatesWith :: (ImmValue -> Either String ImmValue) ->
                        [OpOperand] -> Either String [OpOperand]
adjustImmediatesWith adjimm = mapM adj
  where
    adj (OpndImm imm) = OpndImm <$> adjimm imm
    adj op = Right op

immToW :: ImmValue -> Either String ImmValue
immToW (ImmW imm) = Right (ImmW imm)
immToW (ImmB imm) = Right (ImmW (int imm))
immToW (ImmL imm) | imm < 0x10000 = Right (ImmW (int imm))
immToW imm = Left $ "Value " ++ show imm ++ " is too large for a word"

immToB :: ImmValue -> Either String ImmValue
immToB (ImmB imm) = Right (ImmB imm)
immToB (ImmW imm) | imm < 0x100 = Right (ImmB (int imm))
immToB (ImmL imm) | imm < 0x100 = Right (ImmB (int imm))
immToB imm = Left $ "Value " ++ show imm ++ " is too large for a word"

toHasmPos :: SourcePos -> SrcPos
toHasmPos src = SrcPos (sourceName src) (sourceLine src) (sourceColumn src)
