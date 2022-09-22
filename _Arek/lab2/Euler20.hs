import Data.Char ( digitToInt )

-- Euler 20
-- Time spent : 20min

--function to find the factorial
factorial :: (Num a, Enum a) => a -> a
factorial n = product [1..n]

--function to convert the integer result into a list of integers
digits :: Integer -> [Int]
digits = map digitToInt . show

--calculate the sum of integers of factorial n
euler20 :: Int
euler20 = sum(digits (factorial 100))
