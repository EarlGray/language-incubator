{-
 -  See: http://www.stephendiehl.com/llvm/
 -}

import Data.Word
import qualified Data.Map as M
import Control.Applicative (Applicative(..), (<$>), (<*), (*>))
import Control.Monad.Trans
import Control.Monad.State (gets, modify, MonadState(..), execState)

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import System.Console.Haskeline

import LLVM.General
import qualified LLVM.General.AST as AST
import LLVM.General.AST (functionDefaults, moduleDefinitions,
    defaultModule, Global(..), BasicBlock(..), Module(..),
    {-Function(..),-} Definition(..), Parameter(..), Terminator(..))
import LLVM.General.AST.Type (Type(..), FloatingPointFormat(..))
import LLVM.General.AST.Name
import LLVM.General.AST.Operand (Operand(..))
import LLVM.General.AST.Instruction (Instruction(..))

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

type KName = String

data Expr
    = Float Double
    | BinOp Op Expr Expr
    | Var String
    | KCall KName [Expr]
    | KFunction KName [Expr] Expr
    | Extern KName [Expr]
    deriving (Eq, Ord, Show)

data Op = Plus | Minus | Times | Divide deriving (Eq, Ord, Show)


{- Parser -}

binary s f assoc = Ex.Infix (reservedOp s >> return (BinOp f)) assoc

table = [[binary "*" Times Ex.AssocLeft,
          binary "/" Divide Ex.AssocLeft],
         [binary "+" Plus Ex.AssocLeft,
          binary "-" Minus Ex.AssocLeft]]

int, floating, expr, defp, extern, call, factor :: Parser Expr

int = Float <$> fromInteger <$> integer
floating = Float <$> float
expr = Ex.buildExpressionParser table factor
variable = Var <$> identifier

defp = do
    reserved "def"
    name <- identifier
    args <- parens $ many variable
    body <- expr
    return $ KFunction name args body

extern = do
    reserved "extern"
    name <- identifier
    args <- parens $ many variable
    return $ Extern name args

call = do
    name <- identifier
    args <- parens $ commaSep expr
    return $ KCall name args

factor = try floating
     <|> try int
     <|> try extern
     <|> try defp
     <|> try call
     <|> variable
     <|> parens expr

defn = try extern
    <|> try defp
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


{- Code generator -}
type Names = M.Map String Int

double :: Type
double = FloatingPointType 64 IEEE

type SymbolTable = [(String, Operand)]

data CodegenState
    = CodegenState {
        currentBlock    :: Name,
        blocks          :: M.Map Name BlockState,
        symtab          :: SymbolTable,
        blockCount      :: Int,
        count           :: Word,
        names           :: Names
    } deriving Show

data BlockState
    = BlockState {
        idx   :: Int,
        stack :: [AST.Named Instruction],
        term  :: Maybe (AST.Named Terminator)
    } deriving Show

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
            deriving (Functor, Applicative, Monad, MonadState CodegenState)

newtype LLVM a = LLVM { unLLVM :: State AST.Module a }
            deriving (Functor, Applicative, Monad, MonadState AST.Module)

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM = flip (execState . unLLVM)

emptyModule :: String -> AST.Module
emptyModule label = defaultModule { moduleName = label }

addDefn :: Definition -> LLVM ()
addDefn d = do
    defs <- gets moduleDefinitions
    modify $ \s -> s { moduleDefinitions = defs ++ [d] }

define :: Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body =
    addDefn $ GlobalDefinition $ functionDefaults {
        name = Name Label,
        parameters = ([Parameter ty nm [] | (ty, nm) <- argtys], False),
        returnType = retty,
        basicBlocks = body
    }

external :: Type -> String -> [(Type, Name)] -> LLVM ()
external retty label argtys =
    addDefn $ GlobalDefinition $ functionDefaults {
        name = Name Label,
        parameters = ([Parameter ty nm [] | (ty, nm) <- argtys], False),
        returnType = retty,
        basicBlocks = []
    }

entry :: Codegen Name
entry = gets currentBlock

addBlock :: String -> Codegen Name
addBlock bname = do
    bis <- gets blocks
    ix <- gets blockCount
    nms <- gets names

    let new = emptyBlock ix
        (qname, supply) = uniqueName bname nms

    modify $ \s ->
      s {
        blocks = M.insert (Name qname) new bis,
        blockCount = ix + 1,
        names = supply }
    return $ Name qname

main = parserMain


