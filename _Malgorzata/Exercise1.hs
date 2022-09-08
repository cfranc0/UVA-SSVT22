import Data.List
import Test.QuickCheck 


sumNaturals' :: Integer -> Integer
sumNaturals' n = sum [1..n]

sumNaturals :: Integer -> Integer
sumNaturals n = n*(n+1) `div` 2

testSumNaturals :: Integer -> Bool
testSumNaturals n = let a = abs n in sumNaturals' a == sumNaturals a

sumSquares' :: Integer -> Integer
sumSquares' n = sum [k*k | k <- [1..n]]

sumSquares :: Integer -> Integer
sumSquares n = n*(n+1)*(2*n+1) `div` 6

testSumSquares :: Integer -> Bool
testSumSquares n = let a = abs n in sumSquares' a == sumSquares a