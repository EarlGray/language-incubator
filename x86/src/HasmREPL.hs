import Data.Word
import Text.Printf (printf)
import System.IO (hFlush, stdout)

import HasmTypes
import HasmParse
import HasmCodegen
import X86Opcodes

{-
 - main
 -}
hexBytecode :: [Word8] -> String
hexBytecode = concat . map (printf "%02x ")

mainOpToHex = do
    putStr "**HASM**> " >> hFlush stdout
    op <- readLn :: IO Operation
    putStrLn $ hexBytecode $ bytecode op
    mainOpToHex

mainParseAssemble = do
    putStr "*HASM*> " >> hFlush stdout
    ln <- getLine
    case hasmParseWithSource "stdin" (ln ++ "\n") of
      Left e -> putStrLn $ "Syntax ERROR\n" ++ show e
      Right pstmts ->
        case assembleFromZero pstmts of
          Left e -> putStrLn $ "Codegen ERROR\n" ++ e
          Right (lbldb, hbops) -> do
            putStrLn $ "OK, labels: " ++ show lbldb
            putStrLn $ "OK, code: "
            mapM_ print hbops
    mainParseAssemble

main = mainParseAssemble
