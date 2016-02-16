module Utility where

import Data.Bits
import Type

bit0 = Bit_number 0
bit1 = Bit_number 1
bit2 = Bit_number 2
bit3 = Bit_number 3
bit4 = Bit_number 4
bit5 = Bit_number 5
bit6 = Bit_number 6
bit7 = Bit_number 7
bit8 = Bit_number 8
bit9 = Bit_number 9
bit10 = Bit_number 10
bit11 = Bit_number 11
bit12 = Bit_number 12
bit13 = Bit_number 13
bit14 = Bit_number 14
bit15 = Bit_number 15

size1 = Bit_size 1
size2 = Bit_size 2
size3 = Bit_size 3
size4 = Bit_size 4
size5 = Bit_size 5
size6 = Bit_size 6
size7 = Bit_size 7

-- These helper functions can take advanage of similar ones in Data.Bits.
fetch_bit (Bit_number n) word =
  testBit word n

clear_bit (Bit_number n) word =
  clearBit word n

set_bit (Bit_number n) word =
  setBit word n

-- set_bit and clear_bit require Bit_number, so no need to re-specify it here.
set_bit_to n word value =
  if value then set_bit n word
  else clear_bit n word

fetch_bits (Bit_number high) (Bit_size length) word =
  let mask = complement ((-1) `shiftL` length) in
  (word `shiftR` (high - length + 1)) .&. mask
