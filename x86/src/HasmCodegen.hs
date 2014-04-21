module HasmCodegen where

import Data.Word
import Data.Map ((!))
import qualified Data.Map as M
import qualified Control.Arrow as Arr
import Control.Applicative

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Numeric (showHex)

import HasmTypes
import X86Opcodes

import Control.Exception
import System.IO.Unsafe (unsafePerformIO)

unsafeCatchError :: a -> Either ErrorCall a
unsafeCatchError = unsafePerformIO . try . evaluate 

type MbOpCode = Maybe [Word8]

type Addr = Word32
type Offset = Word32
type CodegenError = String

{-
codegen :: Addr -> [HasmStmtWithPos] -> Either CodegenError (M.Map Section ByteString)
codegen addr stmts = 
  let smap = M.map concatSubsects $ partitionSections stmts
  in 
-}

{-
 - Section map stuff: partitioning statement stream into sections
 -}
type Section = String
type Subsection = Int
type SectionMap = M.Map Section (M.Map Subsection [HasmStmtWithPos])

stmtsForSection :: Section -> SectionMap -> Maybe [HasmStmtWithPos]
stmtsForSection sect smap = concatSubsects <$> M.lookup sect smap

concatSubsects = concat . snd . unzip . M.toAscList

defaultSection = ("text",0)

partitionSections :: [HasmStmtWithPos] -> SectionMap
partitionSections stmts = partitionSections' defaultSection stmts

partitionSections' (sect, subsect) [] = 
  M.singleton sect $ M.singleton subsect []
partitionSections' (sect, subsect) (stmt:stmts) =
  case stmt of
    (HasmStDirective (DirSection newsect newsubsect _), _) ->
      partitionSections' (newsect, newsubsect) stmts
    stmtp -> addStmt $ partitionSections' (sect, subsect) stmts
      where addStmt = M.alter (Just . alterSubsect) sect
            alterSubsect = maybe (M.singleton subsect [stmtp]) (M.alter mbConsStmt subsect)
            mbConsStmt = Just . maybe [stmtp] (stmtp :)

{-
 - Labeling, marking symbols with addresses
 -}

data Label = Label Symbol Addr
instance Show Label where
  show (Label sym addr) = sym ++ "->*0x" ++ showHex addr ""

type LabelDB = M.Map Symbol Addr

{-
 - First pass: 
 -}

firstPass :: (Addr, LabelDB) -> [HasmStmtWithPos] ->
             Either CodegenError (LabelDB, [(HasmStatement, SrcPos, MbOpCode)])
firstPass (addr, lbldb) [] = Right (lbldb, [])
firstPass (addr, lbldb) ((stmt, pos):pstmsts) =
  case stmt of
    HasmStLabel label ->
      case M.lookup label lbldb of
        Nothing -> firstPass (addr, M.insert label addr lbldb) pstmsts
        Just _ -> Left $ show pos ++ ": label redefinition for `" ++ label ++ "'"
    HasmStDirective dir ->
      case dir of
        DirGlobal _ -> firstPass (addr, lbldb) pstmsts
        DirSection _ _ _ -> firstPass (addr, lbldb) pstmsts
        _ -> Left $ show pos ++ ": failed to assemble directive: " ++ show dir
    HasmStInstr prefs oper@(Operation op opnds) ->
      let (oper', mbLbl) = fakeOperation oper
      in case unsafeCatchError (bytecode oper') of
          Right [] -> Left $ show pos ++ ": failed to assemle instruction: " ++ show oper
          Right bs -> case firstPass (addr + (int $ length bs), lbldb) pstmsts of
                  Left e -> Left e
                  Right (lbldb', hbops') ->
                    case mbLbl of
                      Nothing -> Right $ (lbldb', (stmt, pos, Just bs) : hbops')
                      Just _  -> Right $ (lbldb', (stmt, pos, Nothing) : hbops')
          Left e -> Left $ show e

-- if there is a label in the argument, returns (fakeOperation, Just lbl)
-- if the operation is label-free, returns (the_same_operation, Nothing)
fakeOperation :: Operation -> (Operation, Maybe Symbol)
fakeOperation oper@(Operation _ []) = (oper, Nothing)
fakeOperation (Operation op (opnd:opnds)) =
  case opnd of
    OpndRM sib (DisplLabel label) -> 
      (Operation op ((OpndRM sib (Displ32 0)):opnds), Just label)
    _ -> 
      let (Operation _ opnds', mblbl) = fakeOperation (Operation op opnds)
      in (Operation op (opnd:opnds'), mblbl)

secondPass :: Addr -> (LabelDB, [(HasmStatement, SrcPos, MbOpCode)]) ->
              Either CodegenError [(HasmStatement, SrcPos, [Word8])]
secondPass addr (lbldb, mbops) = mapM go mbops -- in Either monad
  where
    go (stmt, pos, Just bs) = Right (stmt, pos, bs)
    go (stmt, pos, Nothing) =
      case stmt of
        HasmStInstr prefs oper -> do
          oper' <- operationLblToAddr lbldb oper
          case unsafeCatchError (bytecode oper') of
              Left e ->   Left $ show e
              Right [] -> Left $ show pos ++ ": failed to assemble instruction: " ++ show oper
              Right bs -> Right$ (stmt, pos, bs)
        _ -> Right $ (stmt, pos, [])

operationLblToAddr :: LabelDB -> Operation -> Either CodegenError Operation
operationLblToAddr lbldb (Operation op opnds) = do
    opnds' <- mapM fixaddr opnds
    return $ Operation op opnds'
  where
    fixaddr opnd@(OpndRM sib displ) = 
      case displ of
        DisplLabel sym -> 
          case M.lookup sym lbldb of
            Just addr -> Right $ OpndRM sib (Displ32 addr)
            Nothing -> Left $ ": failed to resolve label: " ++ sym
        _ -> Right $ opnd
    fixaddr opnd = Right $ opnd

--- temporary test values ----
assembleWithBase addr pstmts = firstPass (addr, M.empty) pstmts >>= secondPass addr
assembleFromZero = assembleWithBase 0

withTestSrc = map (\s -> (s, SrcPos "test.s" 0 0))

