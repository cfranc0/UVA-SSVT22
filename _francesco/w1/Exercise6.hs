-- Time spent: 1h

import Data.List

-- Source Lab1.hs
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

-- counterexamples :: [([Integer], Integer)]
counterexamples = filter (\(primeList, candidate) -> not $ prime candidate) $ map (\x -> (take x primes, (foldl (*) 1 (take x primes) + 1))) [2..]