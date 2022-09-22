import Data.List
import Test.QuickCheck
import Lab1

-- Time spent: 1hr 

--generate primes up to 10000
primes :: [Integer]
primes = 2 : filter prime [3..10000]

-- filters a list to only include the numbers which when reversed are also prime numbers
reversibleStream :: [Integer]
reversibleStream = filter (\x -> Lab1.reversal x `elem` Main.primes) Main.primes

--TESTING
--in order to test this function, the following propert should be checked
-- a) reversibleStream contains only prime numbers

-- test a) 
-- counts all non-prime numbers in a list.. this should return 0
countNonPrimes :: Int
countNonPrimes = length (filter (== False) $ map Lab1.prime reversibleStream)




