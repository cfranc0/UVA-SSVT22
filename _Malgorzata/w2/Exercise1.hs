import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Data.ByteString (count)
import Test.QuickCheck.Test (test)

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