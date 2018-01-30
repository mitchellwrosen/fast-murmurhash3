import Data.Digest.MurmurHash3

import Control.Monad
import Data.ByteString (ByteString)
import Gauge
import System.Random (randomIO)

import qualified Data.ByteString as ByteString
import qualified Data.Hash.Murmur as Murmur3
import qualified Data.Dish.Murmur3 as Dish

main :: IO ()
main =
  defaultMain
    [ env (makeBytes 1000) $ \bytes ->
        bgroup "MurmurHash3_x86_32"
          [ bench "fast-murmurhash3" . flip whnf bytes $
              flip hash32 17
          , bench "Dish" . flip whnf bytes $
              \xs -> head (Dish.murmur3 xs 17 Dish.X86_32)
          , bench "murmur3" . flip whnf bytes $
              Murmur3.murmur3 17
          ]
    ]

makeBytes :: Int -> IO ByteString
makeBytes n = do
  ByteString.pack <$> replicateM n randomIO
