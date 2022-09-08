import Data.List
import Test.QuickCheck 


sumNaturals' :: Integer -> Integer
sumNaturals' n = sum [1..n]

sumNaturals :: Integer -> Integer
sumNaturals n = n*(n+1) `div` 2

testSumNaturals :: Positive Integer -> Bool
testSumNaturals (Positive n) = sumNaturals' n == sumNaturals n

sumSquares' :: Integer -> Integer
sumSquares' n = sum [k*k | k <- [1..n]]

sumSquares :: Integer -> Integer
sumSquares n = n*(n+1)*(2*n+1) `div` 6

testSumSquares :: Positive Integer -> Bool
testSumSquares (Positive n) = sumSquares' n == sumSquares n