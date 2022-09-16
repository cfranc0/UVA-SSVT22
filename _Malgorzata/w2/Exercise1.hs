import Data.List
import System.Random

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1)
    return (p:ps)


testProbs:: [Float] -> (Int, Int, Int, Int)
testProbs xs = (length [x | x <- xs, x > 0, x <= 0.25], 
                length [x | x <- xs, x >= 0.25, x < 0.5],
                length [x | x <- xs, x >= 0.5, x < 0.75],
                length [x | x <- xs, x >= 0.75, x < 1])


main = do
  y <- probs 10000
  return $ testProbs y

  {-
  Time spent: 1,5h
  The function testProbs returns how many numbers are in each quartile. First argument
  of the tuple responds to first quartile (0..0.25), the second argument to the second
  quartile [0.25..0.5) and so on...
  
  results for n = 1 000 000
  1. (249589,250318,249881,250212)
  2. (250216,249582,250077,250125)

  results for n = 100 000
  1. (25219,25021,24966,24794)
  2. (25068,25048,24893,24991)

  results for n = 10 000:
  1. (2541,2519,2396,2544)
  2. (2486,2557,2491,2466) 
  3. (2492,2539,2500,2469)

  results for n = 100
  1. (30,18,24,28)
  2. (19,26,28,27)


If we sum the numbers from each quartile we get the n, so we can say that the function
is probs is working properly. 
We see that the number for each quartile is not exactly 2500 (in case of n = 10000), but this is okay.
For example if we want four random numbers from 1-4, we won't always get the 1,2,3,4.
Everytime every number has the same probability of occurrence.
The bigger the set the greater the chances of a more even distribution.
In mathematics it is called a Continuous uniform distribution.

  -}