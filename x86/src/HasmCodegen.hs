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

data HalfBakedOp = HalfBakedOp {
  hbopcode :: [Word8],
  label :: Maybe Label
} deriving (Show)

oplen :: HalfBakedOp -> Offset
oplen (HalfBakedOp bs lb) = int (length bs) + maybe 0 (const 4) lb

assembleOperations :: (Addr, LabelDB) -> [HasmStmtWithPos] -> 
                      Either CodegenError (LabelDB, [HalfBakedOp])
assembleOperations (_, lbldb) [] = Right (lbldb, [])
assembleOperations (addr, lbldb) ((stmt, pos):rest) =
  case stmt of
    HasmStLabel label -> 
      assembleOperations (addr, M.insert label addr lbldb) rest
    HasmStDirective dir ->
      case dir of
        _ -> Left $ show pos ++ ": failed to assemble directive: " ++ show dir
    HasmStInstr prefs oper@(Operation op opnds) ->
      case bytecode oper of
        [] -> Left $ show pos ++ ": failed to assemble instruction: " ++ show oper
        bs -> Arr.right (\(l, hbops) -> (l, hbop : hbops)) assembled
                where assembled = assembleOperations (addr + oplen hbop, lbldb) rest
                      hbop = HalfBakedOp bs Nothing  {- TODO -}

{-
assembleJumps :: (LabelDB, [HalfBakedOp]) -> ByteString
assembleJumps (lbldb, hbops) = B.pack $ concat $ 
-}

--- temporary test values ----
assembleFromZero = assembleOperations (0, M.empty)
withTestSrc = map (\s -> (s, SourcePos "test.s" 0 0))
-- > assembleOperations (0, M.empty) $ withTestSrc linux_null_s
linux_null_s = [
  HasmStLabel "_start",                                             -- _start:
  HasmStInstr [] (Operation OpMov [OpndImmL 0x0, OpndReg RegEAX]),  --    movl $1, %eax
  HasmStInstr [] (Operation OpMov [OpndImmL 0x1, OpndReg RegEBX]),  --    movl $0, %ebx
  HasmStInstr [] (Operation OpInt [OpndImmB 0x80])               ]  --    int 0x80

label_mov_s = [
  HasmStInstr [] (Operation OpMov [OpndRM (SIB 1 Nothing Nothing) (DisplLabel "x"),
                                   OpndReg RegEAX])] 
