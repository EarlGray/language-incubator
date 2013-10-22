module HasmTypes where

import HasmImports
import X86CPU

type Symbol = String
type Label = String

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
