{-
 -  See: http://www.stephendiehl.com/llvm/
 -}


import Control.Applicative ((<$>), (<*), (*>))
import Control.Monad.Trans

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import System.Console.Haskeline

{- Lexer -}

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    ops = ["+", "-", "*", "/", ";"]
    names = ["def", "extern"]
    style = emptyDef {
              Tok.commentLine = "#",
              Tok.reservedOpNames = ops,
              Tok.reservedNames = names }

integer    = Tok.integer lexer
float      = Tok.float lexer
parens     = Tok.parens lexer
commaSep   = Tok.commaSep lexer
semiSep    = Tok.semiSep lexer
identifier = Tok.identifier lexer
reserved   = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer

type Name = String

data Expr
    = Float Double
    | BinOp Op Expr Expr
    | Var String
    | Call Name [Expr]
    | Function Name [Expr] Expr
    | Extern Name [Expr]
    deriving (Eq, Ord, Show)

data Op = Plus | Minus | Times | Divide deriving (Eq, Ord, Show)


{- Parser -}

binary s f assoc = Ex.Infix (reservedOp s >> return (BinOp f)) assoc

table = [[binary "*" Times Ex.AssocLeft,
          binary "/" Divide Ex.AssocLeft],
         [binary "+" Plus Ex.AssocLeft,
          binary "-" Minus Ex.AssocLeft]]

int, floating, expr, function, extern, call, factor :: Parser Expr

int = Float <$> fromInteger <$> integer
floating = Float <$> float
expr = Ex.buildExpressionParser table factor
variable = Var <$> identifier

function = do
    reserved "def"
    name <- identifier
    args <- parens $ many variable
    body <- expr
    return $ Function name args body

extern = do
    reserved "extern"
    name <- identifier
    args <- parens $ many variable
    return $ Extern name args

call = do
    name <- identifier
    args <- parens $ commaSep expr
    return $ Call name args

factor = try floating
     <|> try int
     <|> try extern
     <|> try function
     <|> try call
     <|> variable
     <|> parens expr

defn = try extern
    <|> try function
    <|> expr

contents p = Tok.whiteSpace lexer *> p <* eof

toplevel = many (defn <* reservedOp ";")

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseTopLevel :: String -> Either ParseError [Expr]
parseTopLevel s = parse (contents toplevel) "<stdin>" s


{- REPL -}

parserREPL :: String -> IO ()
parserREPL line = do
    case parseTopLevel line of
      Left err -> print err
      Right ex -> mapM_ print ex

parserMain = runInputT defaultSettings loop
  where
    loop = do
      minput <- getInputLine "klscp> "
      case minput of
        Nothing -> outputStrLn "bye."
        Just input -> liftIO (parserREPL input) >> loop

main = parserMain
