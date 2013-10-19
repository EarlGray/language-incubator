import X86CPU
import X86Opcodes

import Data.Word
import Text.Printf (printf)
import System.IO (hFlush, stdout)

{-
 - main
 -}
hexBytecode :: [Word8] -> String
hexBytecode = concat . map (printf "%02x ")

main = do
    putStr "**HASM**> " >> hFlush stdout
    op <- readLn :: IO Operation
    putStrLn $ hexBytecode $ bytecode op
    main
