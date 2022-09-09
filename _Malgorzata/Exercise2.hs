import Data.List
import Test.QuickCheck 

testProperty :: Positive Integer -> Bool
testProperty (Positive n) = length (subsequences [1..n]) == 2^n

--Time spent 30mins
-- Similarly to exercise 2, the property wouldln't hold true for negative numbers, 
-- because set can't have negative numbers of elements.
-- It is also hard to test the big numbers, since compiling would take a lot of time (or infinite time).
-- All QuickCheck test cases were successful.