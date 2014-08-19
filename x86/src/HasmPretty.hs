module HasmPretty where

{- pretty-printing -}
import Data.List (intercalate)
import Text.Printf

import HasmTypes

class PrettyPrintable a where
    pretty :: a -> String

instance PrettyPrintable HasmStatement where
  pretty stmt = 
    case stmt of
      HasmStLabel lbl -> lbl ++ ":"
      HasmStDirective dir ->
        case dir of
          DirSection sect nsubsect _ | sect `elem` ["data", "text"] ->
              "." ++ sect ++ " " ++ show nsubsect
          DirSection sect nsubsect _ ->
              ".section " ++ sect ++ ", " ++ show nsubsect
          DirGlobal names -> ".globl " ++ intercalate ", " names
          _ -> "." ++ show dir
      HasmStInstr prefs oper ->
          let s_prefs = intercalate " " (map pretty prefs)
          in s_prefs ++ " " ++ pretty oper
         
instance PrettyPrintable OpPrefix where
    pretty pre =
        case lookup pre dict of
          Just s -> s
          Nothing -> error "No pretty representation for " ++ show pre
      where dict = [ (PreCS, "cs:"), (PreSS, "ss:"), (PreDS, "ds:"), (PreES, "es:"),
                     (PreFS, "fs:"), (PreGS, "gs:"), (PreLock, "lock"), (PreREPNE, "repne"),
                     (PreREPNZ, "repnz"), (PreREP, "rep"), (PreREPE, "repe"), (PreREPZ, "repz") ]

instance PrettyPrintable Operation where
    pretty (Operation instr opnds) = 
        pretty instr ++ " " ++ intercalate ", " (map pretty opnds)
    
instance PrettyPrintable Instr where
    pretty op =
      case lookup op dict of
        Just s -> s
        Nothing -> error "No pretty representation for " ++ show op
      where dict = [
             (OpMov, "mov"), (OpInt, "int"), (OpAdd, "add"), (OpPush, "push"),
             (OpRet, "ret"), (OpLRet, "lret"), (OpCmp, "cmp"), (OpJmp, "jmp" ),
             (OpIMul, "imul") ]

instance PrettyPrintable OpOperand where
    pretty (OpndImm imm) = "$" ++ pretty imm
    pretty (OpndRM sib displ) = pretty displ ++ pretty sib
    pretty (OpndReg reg) = pretty reg

instance PrettyPrintable Register where
    pretty reg = 
      case reg of
        RegL reg -> pretty reg
        RegW reg -> pretty reg
        RegB reg -> pretty reg
        SReg sreg -> pretty sreg

instance PrettyPrintable GPRegister where
    pretty reg = 
      case lookup reg (zip (enumAll :: [GPRegister]) gpRegNames) of
        Just s -> "%" ++ s
        Nothing -> error "No pretty name for " ++ show reg

instance PrettyPrintable GPRegisterW where
    pretty reg =
     case lookup reg (zip (enumAll :: [GPRegisterW]) gpWRegNames) of
       Just s -> "%" ++ s
       Nothing -> error "No pretty name for " ++ show reg

instance PrettyPrintable GPRegisterB where
    pretty reg =
      case lookup reg (zip (enumAll :: [GPRegisterB]) gpBRegNames) of
        Just s -> "%" ++ s
        Nothing -> error "No pretty name for " ++ show reg

instance PrettyPrintable SegRegister where
    pretty sreg =
      case lookup sreg (zip (enumAll :: [SegRegister]) segRegNames) of
        Just s -> "%" ++ s
        Nothing -> error "No pretty name for " ++ show sreg

instance PrettyPrintable ImmValue where
  pretty (ImmL imm) = show ((fromIntegral imm) :: Int32)
  pretty (ImmW imm) = show ((fromIntegral imm) :: Int32)
  pretty (ImmB imm) = show ((fromIntegral imm) :: Int32)

instance PrettyPrintable SIB where
  pretty (SIB _ Nothing  Nothing) = ""
  pretty (SIB _ Nothing  (Just b)) = "(" ++ pretty (RegL b) ++ ")"
  pretty (SIB s (Just i) Nothing)  = "(," ++ pretty i ++ ", " ++ show s ++ ")"
  pretty (SIB s (Just i) (Just b)) = "(" ++ pretty b ++ ", " ++ pretty i ++ ", " ++ show s ++ ")"

instance PrettyPrintable Displacement where
  pretty NoDispl = ""
  pretty (DisplLabel lbl) = lbl
  pretty (Displ32 dspl) = show dspl
  pretty (Displ8  dspl) = show dspl
  

{-
 -  Pretty print the results
 -}
hasmPrettyPrint :: Word32 -> [(HasmStatement, SrcPos, [Word8])] -> String
hasmPrettyPrint _ [] = "\n"
hasmPrettyPrint addr ((stmt, pos, bs) : stmts) = 
  case stmt of
    HasmStInstr prefs oper -> 
      let s_addr = printf "%08x:   " addr
          s_bytes = intercalate " " (map (printf "%02x ") bs) 
          s_stmt = pretty stmt
          s_rest = hasmPrettyPrint (addr + (fromIntegral $ length bs)) stmts
      in s_addr ++ s_bytes ++  "\t;; " ++ s_stmt ++ "\n" ++ s_rest
    HasmStLabel lbl -> show lbl ++ ":\n" ++ hasmPrettyPrint addr stmts
    HasmStDirective dir -> "." ++ show dir ++ "\n" ++ hasmPrettyPrint addr stmts

putPretty = putStr . hasmPrettyPrint 0 

