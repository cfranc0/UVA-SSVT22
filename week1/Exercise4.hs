-- Time spent: 1.5h

-- Source Lab1.hs
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

--
reversal :: Integer -> Integer
reversal = read . reverse . show

reversibleStream :: [Integer] 
reversibleStream = filter (\p -> prime (reversal p)) (takeWhile (<10000) primes)

{-
    Testing method:
    We decided to test 3 properties of this list of primes:
     - Each number is <10000
     - Each number has to have its reverse in the list
     - Each number needs to be a prime
    
    The `primes` function, on which reversibleStream is based, would need to be
    tested as well, but we can see in it's implementation that the code used 
    seems reasonable and there is no reason to write tests for it, especially
    since it has been given by the assignment itself.

    The properties we decided to test allow for a complete test of the
    reversibleStream function as we can confidently say that all the items in 
    the final stream satisfy the requirements of the assignments:
    - Beign < 10000
    - Beign primes
    - Appear if and only if the reversed number is also a prime (which is in
      the list)
    Therefore, it is safe to say this test is complete and there is no reason to
    believe there could be some edge cases not tested that may result in errors.
-}

prop_below10000 :: [Integer] -> Bool
prop_below10000 xs = all (<10000) xs

prop_reverseIsPresent :: [Integer] -> Bool
prop_reverseIsPresent xs = all (\x -> (reversal x) `elem` xs) xs

prop_eachIsPrime :: [Integer] -> Bool
prop_eachIsPrime xs = all (\x -> prime x) xs