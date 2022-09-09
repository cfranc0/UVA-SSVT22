import Data.List
import Test.QuickCheck 

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
   where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

-- We calculate the sum of the first 101 prime numbers. 
-- If the outcome is not a prime number, we take another 101 prime numbers starting from the following prime number.
consecutive101Prime' :: [Integer] -> Integer
consecutive101Prime' (x:xs) 
    | prime k = k
    | otherwise = consecutive101Prime' xs
    where k = x + sum ( take 100 xs)

consecutive101Prime :: Integer
consecutive101Prime = consecutive101Prime' primes

-- Time spent 2h
-- This solution, doesn't require testing. Functions were provided in laboratory materials.
-- We see that the function that checks is the number is prime follows the criteria for a prime number:
-- A number with exactly two factors that is higher than 1.
-- If we used an external function to generate prime numbers or to verify if a number is prime, this would require testing because they could include a bug. 


