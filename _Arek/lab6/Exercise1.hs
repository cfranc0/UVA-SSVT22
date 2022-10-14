module Exercise1 where

import Data.List
import Test.QuickCheck
import SetOrd
import System.IO
import System.Random

-- Time spent: 120 minutes -- started: 11:30

-- Implement a random data generator for the datatype Set Int, 
-- where Set is as defined in SetOrd.hs. First do this from scratch, 
-- next give a version that uses QuickCheck to random test this datatype.

genNums :: Int -> IO (Set Int)
genNums n = do
    nums <- sequence $ replicate n $ randomRIO (minBound, maxBound::Int)
    return $ list2set nums

genNumsQC :: Int -> Gen (Set Int)
genNumsQC n = do
    rList <- sequence [arbitrary :: Gen Int | _ <- [1..n]]
    return $ list2set rList




