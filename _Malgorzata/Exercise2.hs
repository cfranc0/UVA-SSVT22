import Data.List
import Test.QuickCheck 

testProperty :: Positive Integer -> Bool
testProperty (Positive n) = length (subsequences [1..n]) == 2^n

-- Time spent 1h
-- Similarly to exercise 2, the property wouldn't hold true for negative numbers, 
-- because set can't have negative numbers of elements.
-- We are testing part of specification for relatively small numbers
-- since natural numbers are infinite and it would also take a lot of time
-- to compute bigger numbers. We are also not testing if the subsequences function
-- provides correct subsequences.
-- All QuickCheck test cases were successful.