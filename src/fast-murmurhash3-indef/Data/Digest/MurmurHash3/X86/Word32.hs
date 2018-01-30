{-# language BangPatterns #-}
{-# language ScopedTypeVariables #-}

module Data.Digest.MurmurHash3.X86.Word32
  ( hash32
  ) where

import WordArray

import Data.Bits
import Data.Function ((&))
import Data.Word

hash32 :: WordArray -> Word32 -> Word32
hash32 bytes seed =
  bytes
    & fold32 step seed
    & tumble (dock32 bytes)
    & (`xor` (fromIntegral (WordArray.length bytes)))
    & fmix32
 where
  step :: Word32 -> Word32 -> Word32
  step acc n = rotl32 (tumble n acc) 13 * 5 + 0xe6546b64

tumble :: Word32 -> Word32 -> Word32
tumble x y = xor y (rotl32 (x * c1) 15 * c2)

rotl32 :: Word32 -> Int -> Word32
rotl32 x r =
  shiftL x r .|. shiftR x (32 - r)

fmix32 :: Word32 -> Word32
fmix32 x =
  xor z (shiftR z 16)
 where
  y = xor x (shiftR x 16) * 0x85ebca6b
  z = xor y (shiftR y 13) * 0xc2b2ae35

c1 :: Word32
c1 = 0xcc9e2d51

c2 :: Word32
c2 = 0x1b873593
