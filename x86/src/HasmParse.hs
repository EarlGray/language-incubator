module HasmParse where

import Data.Char
import Numeric
import Data.List (intercalate)
import Data.Either (lefts, rights)

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
asmfile = many $ do
  many (linecomment <|> newline)
  optional asmspaces
  pos <- getPosition
  stmt <- asmstmt
  return (stmt, toHasmPos pos)

asmstmt :: Parser HasmStatement
asmstmt = try asmdir <|> try asmlabel <|> asmop <?> "assembly statement"

linecomment = do
  char '#'; many (noneOf "\n"); newline

dirchar = letter <|> char '_'
idschar = letter <|> oneOf "_"
idchar = idschar <|> digit <|> oneOf ".$"

symbol = cons idschar (many idchar)

endstmt = oneOf ";\n"
asmspaces = many1 (oneOf " \t")

cons a b = liftA2 (:) a b
opchar = letter <|> digit
opcode = cons letter (many1 opchar)
regname = cons letter (many1 opchar)

asmdir = do
  char '.'
  dir <- many1 dirchar
  asmspaces
  dirargs <- sepBy (many1 $ noneOf "\n,") (char ',')
  endstmt
  case readDirective dir dirargs of
    Right d -> return $ HasmStDirective d
    Left e -> parserFail e

asmlabel = HasmStLabel <$> (cons idschar (many idchar) <* char ':' <* optional newline) <?> "label"

asmop = do
  op <- opcode
  asmspaces
  opnds <- sepBy asmopnd (char ',' >> optional (oneOf " \t"))
  --optional asmcomment
  endstmt
  case readOperation op opnds of
    Left e -> parserFail e
    Right o -> return $ HasmStInstr [] o

asmopnd :: Parser OpOperand
asmopnd = try (OpndImm <$> asmimm) <|> try (OpndReg <$> asmregister) {-<|> try asmmem-} <?> "opcode operand"

{-
asmmem = do
  mbDspl <- optionMaybe asmdispl
  case mbDspl of
    Just 

asmdispl = 

asmaddr = 
-}

asmimm = do
  char '$'
  num <- readIntegerLit
  return num

asmregister = do
  char '%'
  reg <- regname
  case mbRegByName reg of
    Just r -> return r
    _ -> parserFail $ "Not a register name: " ++ reg
  

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
    glbl | glbl `elem` ["global", "globl"] ->
      let eiSyms = map (parse (symbol <* eof) "") args
      in case lefts eiSyms of
          [] -> Right $ DirGlobal (rights eiSyms)
          (err:_) -> Left $ "failed to parse symbol " ++ show err
    --"long" -> 
    _ -> Left $ "Unknown directive: " ++ dir ++ " " ++ intercalate "," args

readOperation :: String -> [OpOperand] -> Either String Operation
readOperation opname args =
  case opname of
    mov | mov `elem` ["mov", "movl", "movw", "movb"] -> 
      case args of  -- TODO: check types
        [opnd1, opnd2] -> Right $ Operation OpMov args
        _ -> Left $ "mov takes two arguments"
    "ret" -> 
      case args of
        [] ->            Right $ Operation OpRet []
        [OpndImm imm] -> (\immw -> Operation OpRet [OpndImm immw]) `Arr.right` immToW imm
        _ -> Left "ret may take only an optional 16-bit value"
    "int" -> 
      case args of
        [OpndImm imm] -> (\immb -> Operation OpInt [OpndImm immb]) `Arr.right` immToB imm
        _ -> Left $ "int must take a 8-bit interrupt number"
    _ -> Left $ "Unknown operation: " ++ opname

readIntegerLit = do
  firstdig <- digit
  case firstdig of
    d | isDigit d -> do
        ds <- many digit
        case readDec (d:ds) of
          [(num, "")] -> return $ ImmL num
          _ -> parserFail "failed to parse a number"

opndType :: OpOperand -> Maybe ImmValue
opndType (OpndReg (RegL _)) = Just (ImmL 0)
opndType (OpndReg (RegW _)) = Just (ImmW 0)
opndType (OpndReg (SReg _)) = Just (ImmW 0)
opndType (OpndReg (RegB _)) = Just (ImmB 0)
opndType _ = Nothing

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
