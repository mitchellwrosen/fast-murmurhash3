module Data.Digest.MurmurHash3.X86.Word32 ( ) where

import Data.Bits
import Data.Word

rotl64 :: Word64 -> Int -> Word64
rotl64 x r =
  shiftL x r .|. shiftR x (64 - r)

fmix64 :: Word64 -> Word64
fmix64 x =
  xor z (shiftR z 33)
 where
  y = xor x (shiftR x 33) * 0xff51afd7ed558ccd
  z = xor y (shiftR y 33) * 0xc4ceb9fe1a85ec53
