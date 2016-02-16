module Flathead where

import Data.Bits
import Utility

word = 0xBEEF :: Int

fetch_bits_original high length word =
  let mask = complement ((-1) `shiftL` length) in
  (word `shiftR` (high - length + 1)) .&. mask

main = do
  putStrLn (show ((shiftR word 12) .&. (complement (shiftL (-1) 4))))
  print (fetch_bits_original 15 4 word)
  print (fetch_bits bit15 size4 word)
