module Euler14 where
import Data.Foldable (maximumBy)

--Euler 14: Longest Collatz sequence
--bruteforce approach to the problem, takes a while to find the solution but in works in the end
--time started: 17:45
--time ended: 18:56
--time taken: around 1hr

--create a list of integers for a collatz sequence n
collatz :: Integer -> [Integer]
collatz x 
    | x == 1 = [1]
    | odd x = x : collatz(3 * x + 1)
    | otherwise = x : collatz(x `div` 2)

--create a list of tuples (n, number of integers) for every collatz sequence between 1 and 1000000
collatsSeq :: [(Integer, Int)]
collatsSeq = zip [1..1000000] (map length (map collatz [1..1000000]))

--find a tuple with the highest number and return first value only
euler14 :: Integer
euler14 = fst(maximumBy (\a b -> compare (snd a) (snd b)) collatsSeq)