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
import Control.Monad.State
import Control.Monad.Trans.Error

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

emptyLblDb = M.empty

{-
 - First pass:
 -}
type AsmHalfResult = [(HasmStatement, SrcPos, MbOpCode, Addr)]
type AsmResult = [(HasmStatement, SrcPos, [Word8])]
type AsmState = (Addr, LabelDB)

firstPass :: AsmState -> [HasmStmtWithPos] ->
             Either CodegenError (LabelDB, AsmHalfResult)
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
      in case unsafeCatchError (bytecodeWithPos oper' addr) of
          Right [] -> Left $ show pos ++ ": failed to assemle instruction: " ++ show oper
          Right bs -> case firstPass (addr + (int $ length bs), lbldb) pstmsts of
                  Left e -> Left e
                  Right (lbldb', hbops') ->
                    case mbLbl of
                      Nothing -> Right $ (lbldb', (stmt, pos, Just bs, addr) : hbops')
                      Just _  -> Right $ (lbldb', (stmt, pos, Nothing, addr) : hbops')
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

{-
newtype AsmMonad r = AsmMonad { runAsm :: ErrorT CodegenError (State AsmState) r }

liftAsm :: State AsmState a -> AsmMonad a
liftAsm m = AsmMonad (lift m)

--secondPass :: AsmState -> AsmHalfResult -> Either CodegenError AsmResult
secondPass (addr, lbldb) mbops = mapM go mbops
  where
    moveaddr off = liftAsm $ modify (\(addr, lbldb) -> (addr + off, lbldb))
    go (stmt, pos, Just bs) = do
      moveaddr (int $ length bs)
      return (stmt, pos, bs)
    go (stmt, pos, Nothing) = do
      case stmt of
        HasmStInstr prefs oper -> do
          (addr, lbldb) <- liftAsm get
          case operationLblToAddr lbldb oper of
            Right oper' ->
              case unsafeCatchError (bytecodeWithPos oper' addr) of
                Left e -> runAsm $ throwError $ show pos ++ "failed: " ++ show e
                Right [] -> throwError $ show pos ++ ": failed to assemble: " ++ show oper
                Right bs -> do
                  moveaddr (int $ length bs)
                  return (stmt, pos, bs)
            Left e ->
              throwError $ show pos ++ ": can't fix addr: " ++ e
        _ -> return (stmt, pos, [])
-- -}

secondPass :: Word32 -> (LabelDB, AsmHalfResult) -> Either CodegenError AsmResult
secondPass addr (lbldb, mbops) = mapM go mbops -- in Either monad
  where
    go (stmt, pos, Just bs, addr) = Right (stmt, pos, bs)
    go (stmt, pos, Nothing, addr) =
      case stmt of
        HasmStInstr prefs oper -> do
          oper' <- operationLblToAddr lbldb oper
          case unsafeCatchError (bytecodeWithPos oper' addr) of
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


