module HasmImports (
  module Data.Word,
  module Data.Int,
  module Data.Bits,
  int
) where

import Data.Word
import Data.Int
import Data.Bits

int :: (Integral a, Num b) => a -> b
int = fromIntegral

