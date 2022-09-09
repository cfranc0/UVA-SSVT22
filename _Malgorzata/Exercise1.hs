import Data.List
import Test.QuickCheck 

sumSquares' :: Integer -> Integer
sumSquares' n = sum [k*k | k <- [1..n]]

sumSquares :: Integer -> Integer
sumSquares n = n*(n+1)*(2*n+1) `div` 6

testSumSquares :: Positive Integer -> Bool
testSumSquares (Positive n) = sumSquares' n == sumSquares n

sumCubes :: Integer -> Integer
sumCubes n = sum [x^3 | x <- [1..n]]

sumCubes' :: Integer -> Integer
sumCubes' n = ((n*(n+1)) `div` 2)^2

testsumCubes :: Positive Integer -> Bool
testsumCubes (Positive n) = sumCubes n == sumCubes' n

--Time spent 1h
-- In both cases, the properties wouldln't hold true for negative numbers,
-- therefore we limited tested values to only positive numbers.
-- We also know that natural numbers are infinite and it makes it impossible to test 
-- the property for all of them.
-- Hoverwer, all QuickCheck test cases were successful.
