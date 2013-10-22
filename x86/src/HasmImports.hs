module HasmImports (
  module Data.Word,
  module Data.Int,
  module Data.Bits,
  int, safeHead
) where

import Data.Word
import Data.Int
import Data.Bits

int :: (Integral a, Num b) => a -> b
int = fromIntegral

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (s:_) = Just s

