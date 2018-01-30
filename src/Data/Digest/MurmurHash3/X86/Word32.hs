{-# language BangPatterns #-}
{-# language ScopedTypeVariables #-}

module Data.Digest.MurmurHash3.X86.Word32
  ( hash32
  ) where

import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Data.Coerce (coerce)
import Data.Word
import Foreign.Storable (peek, peekElemOff)
import Foreign.Ptr (Ptr, plusPtr)
import System.IO.Unsafe (unsafeDupablePerformIO)

import qualified Data.ByteString as ByteString

hash32 :: ByteString -> Word32 -> Word32
hash32 bytes seed = fmix32 (xor k (fromIntegral (ByteString.length bytes)))
 where
  k :: Word32
  k =
    unsafeDupablePerformIO .  unsafeUseAsCString bytes $ \ptr -> do
      let loop :: Word32 -> Int -> IO Word32
          loop !acc i
            | i == nblocks = pure acc
            | otherwise = do
                k1 :: Word32 <-
                  peekElemOff (coerce ptr) i
                loop (rotl32 (tumble k1 acc) 13 * 5 + 0xe6546b64) (i+1)

      k1 :: Word32 <-
        loop seed 0

      let ptail :: Ptr Word8
          ptail = coerce ptr `plusPtr` (nblocks * 4)

      case ntail of
        0 -> pure k1
        1 -> do
          x :: Word8 <-
            peek ptail
          pure (tumble (fromIntegral x) k1)
        2 -> do
          x :: Word32 <- do
            b0 :: Word8 <-
              peek ptail
            b1 :: Word8 <-
              peek (ptail `plusPtr` 1)
            pure (shiftL (fromIntegral b1) 8 .|. fromIntegral b0)
          pure (tumble x k1)
        3 -> do
          x :: Word32 <- do
            b0 :: Word8 <-
              peek ptail
            b1 :: Word8 <-
              peek (ptail `plusPtr` 1)
            b2 :: Word8 <-
              peek (ptail `plusPtr` 2)
            pure
              (shiftL (fromIntegral b2) 16
                .|. shiftL (fromIntegral b1) 8
                .|. fromIntegral b0)
          pure (tumble x k1)
        _ ->
          undefined

  (nblocks, ntail) = ByteString.length bytes `divMod` 4

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
