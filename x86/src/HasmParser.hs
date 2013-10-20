module HasmParser where

import Data.Char
import Data.List
import Numeric (readHex, readOct, readDec)
import Data.Maybe (fromMaybe, fromJust)

import HasmImports
import X86CPU

safeHead [] = Nothing
safeHead (s:_) = Just s

type Symbol = String
type Len = Int

data Directive
  -- location control directives:
  = DirBAlign Int Int Int -- Align by Int bytes with pattern Int(=0) no more than Int(=INT_MAX)
  | DirSection String Int String  -- section with name:String and opt. subsection index Int(=0), opt. flags:String
  | DirFile String        -- set filename
  | DirInclude String   -- include file
  | DirSkip Int Word8  -- skip n:Int bytes filling with value:Word8(=0); synonym: .space
  | DirOrg Int
-- symbol control:
  | DirEqu Symbol Int   -- set value of the symbol; synonym: .set
  | DirEquiv Symbol Int   -- as .equ, but signal an error if Symbol is already defined
  | DirEqv Symbol Int   -- lazy assignment
  | DirSize Symbol Int  -- set symbol size
  | DirType Symbol String -- set symbol type
  -- symbol visibility:
  | DirExtern
  | DirGlobal [Symbol]    -- .global/.globl
  | DirHidden [Symbol]
  | DirLocal [Symbol]
  | DirWeak [Symbol]
  -- data directives:
  | DirBytes [Word8]
  | DirShort [Word16]   -- .hword/.short/.octa
  | DirWord [Word32]  -- .word/.long
  | DirAscii [[Word8]]  -- zero or more ASCII strings
  | DirAsciz [[Word8]]  -- zero or more ASCII strings separated by \0, synonym: .string
  | DirDouble [Double]  -- zero or more flonums
  | DirFloat [Float]  -- synonyms: .single
  | DirFill Int Int Int -- repeat times:Int pattern of size:Int(=1) (if >8 than 8) of value:Int(=0)
  | DirComm Symbol Int Int -- make a BSS symbol:Symbol with length:Int and alignment:Int(=1)
  | DirCFI CFIInfo
  -- def directives:
  | DirDef Symbol     -- start defining debug info for Symbol
  | DirDim
  | DirEndef
  -- conditional assembly directives:
  | DirIf Int 
  | DirIfdef Symbol
  | DirIfblank String 
  | DirIfcmp String String  -- .ifc/.ifeqs
  | DirIfeq Int Int
  | DirIfge Int Int
  | DirIfgt Int Int
  | DirElse
  | DirElseIf
  | DirEndif
  -- listing directives:
  | DirErr  
  | DirError String
  | DirEnd  -- marks end of the assembly file, does not process anything from this point
  | DirEject  -- generate page break on assembly listings
  deriving (Show)

data HasmStatement
  = HasmStLabel String
  | HasmStDirective Directive
  | HasmStInstr [Maybe OpPrefix] Operation 
  deriving (Show)

{-
hasmParse :: String -> [HasmStatement]
hasmParse = parseTokens . hasmLexer
-}

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
readCommand (t:ts) =
  case t of
    --mov | "mov" `isPrefixOf` mov ->
    TokSymbol "int" ->
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
    TokSymbol op -> error $ "unexpected opcode: " ++ op
    tok -> error $ "unexpected token " ++ show tok
    {-"jmp" ->
        case safeHead ts of
          Just (TokSymbol label) -> 
    -}
               
readListOfSymbols :: [Token] -> [([Symbol], [Token])]
readListOfSymbols ts = readlst [] ts
  where
    readlst lst (TokSymbol sym : ts) =
      case safeHead ts of
        Just TokSep -> [(reverse lst, tail ts)]
        Just TokComma -> readlst (sym : lst) (tail ts)
        _ -> error "readListOfSymbols: unexpected end-of-stream"

{-
 - Call Frame Info directives
 -}
data CFIInfo
  = CFISections [String]
  | CFIStartproc
  | CFIEndproc
  | CFIPersonality
  | CFILsda
  | CFIDefCfa
  | CFIDefCfaReg Int
  | CFIDefCfaOffset Int
  | CFIAdjCfaOffset Int
  | CFIOffset Int Int
  | CFIRelOffset
  | CFIRegister Int Int
  | CFIRestore Int
  | CFIUndefined Int
  | CFISameValue Int
  | CFIRememberState
  | CFIRetColumn Int
  | CFISignalFrame
  | CFIEscape 
  deriving (Show, Eq)
