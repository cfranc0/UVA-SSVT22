import Data.List
import Test.QuickCheck 

testProperty :: Positive Integer -> Bool
testProperty (Positive n) = length (subsequences [1..n]) == 2^n
