import Data.List
import Test.QuickCheck

testProperty :: Positive Int -> Bool
testProperty (Positive n) = length (permutations [1..n]) == product [1..n]
