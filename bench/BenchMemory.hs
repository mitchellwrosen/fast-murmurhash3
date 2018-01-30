import Data.Digest.MurmurHash3

import Data.ByteString (ByteString)
import Weigh

import qualified Data.ByteString as ByteString
import qualified Data.Hash.Murmur as Murmur3
import qualified Data.Dish.Murmur3 as Dish

main :: IO ()
main =
  mainWith weigh

weigh :: Weigh ()
weigh = do
  func "fast-murmurhash3" (flip hash32 17) bytes
  func "Dish" (\xs -> head (Dish.murmur3 xs 17 Dish.X86_32)) bytes
  func "murmur3" (Murmur3.murmur3 17) bytes

bytes :: ByteString
bytes = ByteString.pack (replicate 1000000 1)
