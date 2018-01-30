import Data.Digest.MurmurHash3

import Test.QuickCheck

import qualified Data.ByteString as ByteString
import qualified Data.Hash.Murmur as Murmur3
import qualified Data.Dish.Murmur3 as Dish

main :: IO ()
main = do
  quickCheck (withMaxSuccess 10000 prop_same_as_dish)
  quickCheck (withMaxSuccess 10000 prop_same_as_murmur3)

prop_same_as_dish :: Property
prop_same_as_dish =
  forAll arbitrary $ \seed ->
    forAll arbitrary $ \bytes ->
      let
        bytes' = ByteString.pack bytes
        actual = hash32 bytes' seed
        expected =
          fromIntegral
            (head (Dish.murmur3 bytes' (fromIntegral seed) Dish.X86_32))
      in
        actual === expected

prop_same_as_murmur3 :: Property
prop_same_as_murmur3 =
  forAll arbitrary $ \seed ->
    forAll arbitrary $ \bytes ->
      let
        bytes' = ByteString.pack bytes
        actual = hash32 bytes' seed
        expected = Murmur3.murmur3 seed bytes'
      in
        actual === expected
