module HasmParser where

import Data.Char
import Data.List
import Numeric (readHex, readOct, readDec)
import Data.Maybe (fromMaybe, fromJust)

import HasmTypes
import HasmImports
import X86CPU

hasmParse :: String -> [HasmStatement]
hasmParse = parseTokens . hasmLexer

{-
 - Lexer
 -}
data Token
  = TokChar Char      -- '(', '*', etc
  | TokSep            -- ';' or newline
  | TokComma          -- ','
  | TokLabel String   
  | TokRegister String
  | TokSymbol String  
  | TokLitString String -- "..."
  | TokImmInt Int     -- $<val>
  | TokInt Int        -- 4, 0xdeadbeef, etc
  deriving (Show, Eq)


hasmLexer :: String -> [Token]

hasmLexer "" = []
hasmLexer (',':ss) = TokComma : hasmLexer ss
-- skip comments
hasmLexer ('#':str) = hasmLexer $ dropWhile (/= '\n') str
hasmLexer ('/':'*':str) = hasmLexer $ skipTo "*/" str
  where skipTo end str | end `isPrefixOf` str = drop (length end) str
        skipTo end (_:str) = skipTo end str

-- string literals
hasmLexer ('"':ss) = TokLitString str : hasmLexer rest
  where (str, rest) = hasmReadString "" ss

hasmLexer ('%':s:ss) | isAlpha s = TokRegister (s:reg) : hasmLexer rest
  where (reg, rest) = span isAlphaNum ss
hasmLexer ('%':ss) = TokChar '%' : hasmLexer ss

-- TokImmInt
hasmLexer ('$':ss) = 
  case hasmReadInt ss of
    [(num, rest)] -> TokImmInt num : hasmLexer rest
    _ -> TokChar '$' : hasmLexer ss

hasmLexer (s:ss) 
  | s `elem` symStart = 
    let (symbol, rest) = spanSym ss
        spanSym = span (`elem` symChars) 
        symChars = symStart ++ ['0'..'9'] ++ "$"
    in  if safeHead rest == Just ':'
        then TokLabel (s:symbol) : hasmLexer (tail rest)
        else TokSymbol (s:symbol) : hasmLexer rest
  | s `elem` " \t" = hasmLexer ss       -- skip whitespaces
  | s `elem` ";\n" = TokSep : hasmLexer ss  -- separators
  | otherwise = 
        case hasmReadInt (s:ss) of
          [(num, rest)] -> TokInt num : hasmLexer rest
          _ ->  TokChar s : hasmLexer ss

symStart = ['A'..'Z'] ++ ['a'..'z'] ++ "_."


hasmReadInt :: String -> [(Int, String)]
hasmReadInt ('0':s:ss)
  | s `elem` "bB" = readBin ss
  | s `elem` "xX" = readHex ss
  | isOctDigit s = readOct (s:ss)
hasmReadInt (s:ss)
  | isDigit s = readDec (s:ss)
hasmReadInt ('-':ss) =
  case hasmReadInt ss of
    [(num, rest)] -> [(-num, rest)]
    _ -> []
hasmReadInt _ = []

readBin :: String -> [(Int, String)]
readBin "" = []
readBin ss | head ss `elem` "01" = readBin' 0 ss
  where 
    readBin' n ('0':ss) = readBin' (2*n) ss
    readBin' n ('1':ss) = readBin' (2*n + 1) ss
    readBin' n ss = [(n, ss)]
readBin _ = []

hasmReadString :: String -> String -> (String, String)
hasmReadString str ('"':ss) = (reverse str, ss)
hasmReadString str ('\\':s:ss)
  | isOctDigit s =  -- try to convert 3 symbols as octal code:
      let (octs, rest) = splitAt 2 ss
      in case readOct (s : octs) of
          [(num, "")] -> hasmReadString (chr num : str) rest
          _ -> hasmReadString (s:str) ss
  | 'x' == s = -- try to convert all following hexadecimal chars as hex code:
      case readHex ss of
        [] -> hasmReadString ('x':str) ss -- gloss it over
        [(num, ss')] -> hasmReadString (chr num : str) ss'
  | otherwise = hasmReadString (s':str) ss
      where s' = fromMaybe s $ lookup s (zip "bfnrt" "\b\f\n\r\t")
hasmReadString str (s:ss) = hasmReadString (s:str) ss

{-
 -  Parse tokens
 -}
parseTokens :: [Token] -> [HasmStatement]

parseTokens [] = []
parseTokens (TokSep : ts) = parseTokens ts
parseTokens (TokSymbol sym : ts) =
  case sym of
    ('.':dir) -> let (stmt, ts') = readDirective dir ts
                 in stmt : parseTokens ts'
    _ -> let (stmt, ts') = readCommand (TokSymbol sym : ts)
         in stmt : parseTokens ts'
parseTokens (TokLabel label : ts) =
  HasmStLabel label : parseTokens ts    -- TODO: check if there are such labels
parseTokens (t:ts) = error $ "Error: unexpected '" ++ show t ++ "' in the program"


readDirective :: String -> [Token] -> (HasmStatement, [Token])
readDirective dir ts = 
  case dir of
    d | d `elem` ["data", "text"] -> 
            case safeHead ts of
              Just (TokInt n) -> (HasmStDirective (DirSection ('.':d) n ""), drop 2 ts)
              Just TokSep -> (HasmStDirective (DirSection ('.':d) 0 ""), tail ts)
              _ -> let unexptd = maybe "end-of-stream" show $ safeHead ts
                   in error $ d ++ " directive must be followed by optional subsection num and separator, unexpected " ++ unexptd
    "section" ->
       case safeHead ts of
         Just (TokSymbol name) -> 
            case safeHead (tail ts) of
              Just TokSep -> (HasmStDirective (DirSection name 0 ""), drop 2 ts)
              Just tok -> error $ "unexpected .section argument: " ++ show tok
              _ -> error $ "Unexpected end-of-stream"
         Nothing -> error $ "unexpected end-of-stream"
    d | d `elem` ["globl", "global"] ->
        case readListOfSymbols ts of
          [(syms, ts')] -> (HasmStDirective (DirGlobal syms), ts')
          _ -> error ".global: a list of symbols expected"
    _ -> error $ "Unknown directive: ." ++ dir   

readCommand :: [Token] -> (HasmStatement, [Token])
readCommand (TokSymbol sym : ts) =
  case sym of
    {-mov | "mov" `isPrefixOf` mov ->
      case drop (length "mov") mov of
        "l" -> 
               in (HasmStInstr (OpMov op1 op2), ts')
      -}
    "int" ->
        case safeHead ts of
          Just (TokImmInt imm) ->
            if imm < 0 || imm > 0xFF 
            then error "interrupt number must be less than 0x100"
            else case safeHead (tail ts) of
                   Just TokSep -> (HasmStInstr [] (OpInt (OpndImmB $ int imm)), drop 2 ts)
                   Just tok -> error $ "command is not delimited: " ++ show tok
                   Nothing -> (HasmStInstr [] (OpInt (OpndImmB $ int imm)), [])
                   -- TODO: handle prefixes
          _ -> error $ "int must take an intr number, found " ++ show (safeHead ts)
    op -> error $ "unexpected opcode: " ++ op
readCommand (t:ts) = error $ "unexpected token " ++ show t

{-
readOpnd :: [Token] -> (OpOperand, [Token])
readOpnd (t:ts) =
  case t of
    TokSymbol sym -> 
      case head ts of
        TokChar '(' -> 
          let (sib, ts') = readSIB ts 
          in (OpndRM sib (DisplLabel sym), ts')
        _ -> (OpndRM noSIB (DisplLabel sym), ts)
    TokChar '(' ->
      let (sib, ts') = readSIB ts
      in (OpndRM sib NoDispl, ts')
    TokRegister rstr -> (readReg rstr, ts)
    TokImmInt num ->
    TokInt num -> 
  where 
    readSIB (t:ts) =
      let (base, t') = case t of
                  TokRegister rstr -> 
                    case head ts of
                      TokComma -> (regByName rstr, tail ts)
                      _ -> "Comma is expected after base in SIB"
                  TokComma -> (Nothing, tail ts)
                  _ -> error "Comma or register is expected in SIB"
          ind = case 
-}

readReg :: String -> GPRegister
readReg rstr = 
  case regByName rstr of
    Just reg -> reg
    _ -> error $ "Unknown operand: " ++ rstr
               
readListOfSymbols :: [Token] -> [([Symbol], [Token])]
readListOfSymbols ts = readlst [] ts
  where
    readlst lst (TokSymbol sym : ts) =
      case safeHead ts of
        Just TokSep -> [(reverse lst, tail ts)]
        Just TokComma -> readlst (sym : lst) (tail ts)
        _ -> error "readListOfSymbols: unexpected end-of-stream"

