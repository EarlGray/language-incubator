module HasmParse where

import Data.List (intercalate)

import Control.Applicative ((<$>), (<*), liftA2)
import Control.Monad (when)

import Text.Parsec
import Text.Parsec.String (Parser)

import HasmImports
import HasmTypes
import X86CPU

type ParseResult = [(HasmStatement, SourcePos)]

hasmParseFile :: FilePath -> IO (Either ParseError ParseResult)
hasmParseFile fname = hasmParseWithSource fname <$> readFile fname

hasmParseWithSource :: FilePath -> String -> Either ParseError ParseResult
hasmParseWithSource fname ss = parse asmfile fname ss

asmfile :: Parser ParseResult
asmfile = many $ do
  optional asmspaces
  pos <- getPosition
  stmt <- asmstmt
  return (stmt, pos)

asmstmt :: Parser HasmStatement
asmstmt = try asmdir <|> try asmlabel <|> asmop <?> "assembly statement"

dirchar = letter <|> char '_'
idschar = letter <|> oneOf "_"
idchar = idschar <|> digit <|> oneOf ".$"

endstmt = oneOf ";\n"
asmspaces = many1 (oneOf " \t")

cons a b = liftA2 (:) a b
opchar = letter <|> digit
opcode = cons letter (many1 opchar)
regname = cons letter (many1 opchar)

asmdir = do
  char '.'
  dir <- many1 dirchar
  dirargs <- sepBy (many1 $ noneOf "\n,") (char ',') <* endstmt
  case readDirective dir dirargs of
    Right d -> return $ HasmStDirective d
    Left e -> parserFail e

asmlabel = HasmStLabel <$> cons idschar (many1 idchar) <* char ':' <?> "label"

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
asmopnd = try (OpndReg <$> asmregister) {-<|> try asmaddr <|> try asmmem-} <?> "opcode operand"

{-
asmmem = do
  mbDspl <- optionMaybe asmdispl
  case mbDspl of
    Just 

asmdispl = 

asmaddr = 
-}

asmregister = do
  char '%'
  reg <- regname
  case regByName reg of
    Just r -> return r
    _ -> parserFail $ "Not a register name: " ++ reg
  

readDirective :: String -> [String] -> Either String Directive
readDirective dir args =
  case dir of
    _ -> Left $ "Unknown directive: " ++ dir ++ intercalate "," args

readOperation :: String -> [OpOperand] -> Either String Operation
readOperation opname args =
  case opname of
    "mov" -> Right $ Operation OpMov args
    _ -> Left $ "Unknown operation: " ++ opname
