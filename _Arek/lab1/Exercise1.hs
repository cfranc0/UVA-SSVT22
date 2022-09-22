import Data.List
import Test.QuickCheck    
import Lab1

-- Time spent: 2hours (0 haskell experience)

-- function to compare the equation to the sum of integers 1..n (question 1)
q1 :: Integral a => a -> Bool
q1 x = (x*(x+1)) `div` 2 == sum [1..x]

q2 :: Integral a => a -> Bool
q2 x = (x*(x+1))*(2*x+1) `div` 6 == sumofsquares x

q3 :: Integral a => a -> Bool
q3 x = ((x*(x+1)) `div` 2)^2 == sumoftriangles x

-- helper function to calculate the sum of squares
sumofsquares :: (Num a, Enum a) => a -> a
sumofsquares x = sum $ map (^2) [1..x]

-- helper function to calculate the sum of squares
sumoftriangles :: (Num a, Enum a) => a -> a
sumoftriangles x = sum $ map (^3) [1..x]

-- function to generate naturals numbers only 
-- https://matt.might.net/articles/quick-quickcheck/
naturals :: Gen Integer
naturals = 
 do 
    x <- arbitrary 
    if x == 0 
      then return 1
    else if x < 0
      then return (-x)
    else 
      return x

-- to test with quickcheck 
--  quickCheck (forAll naturals qx) where x is q number