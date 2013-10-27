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

data HalfBakedOp 
    = WholeOp [Word8]          
    | HoleOp HasmStatement Symbol 
  deriving (Show, Read, Eq)     

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
             Either CodegenError (LabelDB, [HalfBakedOp])
firstPass (addr, lbldb) [] = Right (lbldb, [])
firstPass (addr, lbldb) ((stmt, pos):pstmsts) =
  case stmt of
    HasmStLabel label ->
      case M.lookup label lbldb of
        Nothing -> firstPass (addr, M.insert label addr lbldb) pstmsts
        Just _ -> Left $ show pos ++ ": label redefinition for `" ++ label ++ "'"
    HasmStDirective dir ->
      case dir of
        _ -> Left $ show pos ++ ": failed to assemble directive: " ++ show dir
    HasmStInstr prefs oper@(Operation op opnds) ->
      let (oper', mbLbl) = fakeOperation oper
      in case bytecode oper' of
          [] -> Left $ show pos ++ ": failed to assemle instruction: " ++ show oper
          bs -> case firstPass (addr + (int $ length bs), lbldb) pstmsts of
                  Left e -> Left e
                  Right (lbldb', hbops') ->
                    case mbLbl of
                      Nothing -> Right $ (lbldb, WholeOp bs : hbops')
                      Just label -> Right $ (lbldb, HoleOp stmt label : hbops')

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
secondPass :: Addr -> LabelDB -> [(HalfBakedOp, HasmStatement)] ->
              [HalfBakedOp]
secondPass addr lbldb 
-}

--- temporary test values ----
assembleFromZero = firstPass (0, M.empty)
withTestSrc = map (\s -> (s, SourcePos "test.s" 0 0))

-- > assembleFromZero $ withTestSrc linux_null_s
movinstr = HasmStInstr [] . Operation OpMov 
intinstr = HasmStInstr [] . Operation OpInt
linux_null_s = [
  HasmStLabel "_start",                           -- _start:
  movinstr [OpndImm (ImmL 0x0), OpndReg (RegL RegEAX)],  --    movl $1, %eax
  movinstr [OpndImm (ImmL 0x1), OpndReg (RegL RegEBX)],  --    movl $0, %ebx
  intinstr [OpndImm (ImmB 0x80)]               ]  --    int 0x80

label_mov_s = [
  movinstr [OpndRM (SIB 1 Nothing Nothing) (DisplLabel "x"),
                                   OpndReg (RegL RegEAX)]] 

loop_jmp_s = [
  HasmStLabel "_start",
  HasmStLabel "loop_start",
  HasmStInstr [] $ Operation OpJmp [OpndRM noSIB (DisplLabel "loop_start")] ]
