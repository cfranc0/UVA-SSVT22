import Data.List
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs where xs = takeWhile (\ y -> y^2 <= n) primes


primes :: [Integer]
primes = 2 : filter prime [3..] 


minimum' :: (Ord a) => [a] -> a  
minimum' [] = error "maximum of empty list"  
minimum' [x] = x  
minimum' (x:primes)   
    | x < maxTail = x  
    | otherwise = MinTail  
    where MinTail = mimimum' primes  
