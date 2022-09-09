import Data.List
import Test.QuickCheck 

testProperty :: Positive Integer -> Bool
testProperty (Positive n) = length (permutations n) == product [1..n]
