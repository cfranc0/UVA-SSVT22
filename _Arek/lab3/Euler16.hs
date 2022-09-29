module Euler16 where
import Data.Char (digitToInt)

--Euler 16: Power digit sum
-- calculate the power first, turn into a list of integers, apply sum() to the list
--time started: 20:06
--time ended: 20:20  
--time taken: 15mins

--function to convert the integer result into a list of integers
digits :: Integer -> [Int]
digits = map digitToInt . show

--
euler16 :: Int
euler16 = sum(digits(2^1000))
