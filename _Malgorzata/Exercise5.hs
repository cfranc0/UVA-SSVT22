import Data.List
import Test.QuickCheck 

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
   where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

consecutive101Prime' :: [Integer] -> Integer
consecutive101Prime' (x:xs) 
    | prime k = k
    | otherwise = consecutive101Prime' xs
    where k = x + sum ( take 100 xs)

consecutive101Prime :: Integer
consecutive101Prime = consecutive101Prime' primes

-- This solution, in my opinion, doesn't require testing. We calculate the sum of the first 101 prime numbers. If the outcome is not a prime number, we start from the following prime number.
-- If we used an external function to generate prime numbers or verify if a number is prime, this would require testing because they may include a problem.
-- However, it is clear that the presented function complies with the criteria for generating prime numbers.