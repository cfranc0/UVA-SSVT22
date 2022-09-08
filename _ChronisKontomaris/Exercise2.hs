import Test.QuickCheck


import Data.List

sumcube :: Integer -> Integer 
sumcube n= sum [k^3 | k <- [1..n]]


sumcube' :: Integer ->Integer
sumcube' k =(k*(k+1)*(2*k+1) `div` 2)^2

test :: Integer -> Bool
test n = sumcube' n == sumcube n




