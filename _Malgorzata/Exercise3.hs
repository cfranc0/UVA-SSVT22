import Data.List
import Test.QuickCheck

testProperty :: Positive Int -> Bool
testProperty (Positive n) = length (permutations [1..n]) == product [1..n]

-- Time spent 30mins
-- Like in exercise 1 and 2, the property wouldln't hold true for negative numbers, 
-- because list can't have negative numbers of elements.
-- We are testing part of specification for relatively small numbers
-- since natural numbers are infinite and it would also take a lot of time
-- to compute bigger numbers. We are also not testing if the permutations function
-- provides correct subsequences, or the product function returns correct value.
-- All QuickCheck test cases were successful.