module HasmParse where

import Text.Parsec
import Text.Parsec.String (Parser)

import HasmImports
import HasmTypes

import Control.Applicative ((<$>))
import Control.Monad (when)
import Text.Parsec

hasmParseFile :: FilePath -> IO (Either ParseError [HasmStatement])
hasmParseFile fname = hasmParseWithSource fname <$> readFile fname

hasmParseWithSource :: FilePath -> String -> Either ParseError [HasmStatement]
hasmParseWithSource fname ss = parse asmfile fname ss

asmfile :: Parser [HasmStatement]
asmfile = many asmstmt

asmstmt :: Parser HasmStatement
asmstmt = try asmdir <|> try asmlabel <|> asmop

dirchar = letter <|> char '_'
idschar = letter <|> oneOf "_"
idchar = idschar <|> digit <|> oneOf ".$"

endstmt = oneOf ";\n"
asmspaces = many1 (oneOf " \t")

cons a b = lift2 (:) a b
opchar = letter <|> digit
opcode = cons letter (many1 opchar)
regname = cons letter (many1 opchar)

asmdir = do
  char '.'
  dir <- many1 dirchar

asmlabel = cons idschar (many1 idchar) <* char ':' <?> "label"

asmop = do
  op <- opcode
  when (not (isOpcode op)) $ parserFail ("Unknown opcode: " ++ op)

  asmspaces
  opnds <- sepBy (char ',' >> optional (oneOf " \t")) asmopnd
  optional asmcomment
  endstmt
  return $ mkOp op opnds

asmopnd :: Parser OpOperand
asmopnd = try asmregister <|> try asmaddr <|> try asmmem <?> "opcode operand"

asmmem = do
  mbDspl <- optionMaybe asmdispl
  case mbDspl of
    Just 

asmdispl = 

asmaddr = 

asmregister :: NamedRegister a => String -> a
asmregister = do
  char '%'
  reg <- regname
  case regByName reg of
    Just r -> r
    _ -> parserFail $ "Not a register name: " ++ reg
  
