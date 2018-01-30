{-# language BangPatterns #-}
{-# language ScopedTypeVariables #-}

module WordArray where

import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeIndex, unsafeUseAsCString)
import Data.Coerce (coerce)
import Data.Word
import Foreign.Storable (peekElemOff)
import System.IO.Unsafe (unsafeDupablePerformIO)

import qualified Data.ByteString as ByteString

type WordArray
  = ByteString

fold32 :: (a -> Word32 -> a) -> a -> ByteString -> a
fold32 f z bytes =
  unsafeDupablePerformIO $
    unsafeUseAsCString bytes $ \ptr ->
      let loop !acc n
            | n == nwords =
                pure acc
            | otherwise = do
                w :: Word32 <-
                  peekElemOff (coerce ptr) n
                loop (f acc w) (n+1)
      in
        loop z 0
 where
  nwords =
    ByteString.length bytes `div` 4

dock32 :: ByteString -> Word32
dock32 bytes =
  case len `mod` 4 of
    0 ->
      0
    1 ->
      fromIntegral (unsafeIndex bytes (len-1))
    2 ->
      shiftL (fromIntegral (unsafeIndex bytes (len-1))) 8
        .|. fromIntegral (unsafeIndex bytes (len-2))
    3 ->
      shiftL (fromIntegral (unsafeIndex bytes (len-1))) 16
        .|. shiftL (fromIntegral (unsafeIndex bytes (len-2))) 8
        .|. fromIntegral (unsafeIndex bytes (len-3))
    _ ->
      undefined
 where
  len = ByteString.length bytes

length :: ByteString -> Int
length =
  ByteString.length
