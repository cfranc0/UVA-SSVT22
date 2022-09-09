import Data.List
import Test.QuickCheck

testProperty :: Positive Int -> Bool
testProperty (Positive n) = length (permutations [1..n]) == product [1..n]

-- Time spent 30mins
-- Like in exercise 1 and 2, the property wouldln't hold true for negative numbers, 
-- because list can't have negative numbers of elements.
-- It is also hard to test the big numbers, since compiling would take a lot of time (or infinite time).
-- TODO what do we test here?