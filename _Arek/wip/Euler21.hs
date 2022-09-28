module Euler21 where

--Euler 21: Amicable numbers
{-
Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.
For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; 
so d(284) = 220.
Evaluate the sum of all the amicable numbers under 10000.
-}
--time started: 21:32
--time ended: 22:14
--time taken: 45 min

--function to find all divisors of a number
divisors :: Integral a => a -> [a]
divisors n = [x | x <- [1..(n-1)], n `rem` x == 0]

--sum all divisors
sumdivisors :: Integral a => a -> a
sumdivisors n = sum (divisors n)

--check if number is a amicable number
isAmical :: Integral a => a -> Bool
isAmical x 
    | x == sumdivisors(sumdivisors x) && sumdivisors x /= x = True
    | otherwise = False

--find all amicable numbers under a 10000
areAmicals :: [Integer]
areAmicals = [x | x <- [1..10000], isAmical x]

--sum all amicable numbers
euler21 :: Integer
euler21 = sum areAmicals