module Exercise4 where

-- Time spent: x minutes -- started: 13.05

-- Implement a function that calculates the strength of a given set of properties, which is the percentage of mutants they kill.

import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable ( multiplicationTable, mProp )

type PropertyList = [[Integer] -> Integer -> Bool]
type Mutator = [Integer] -> Gen [Integer]

{-
--testAll :: PropertyList -> Mutator -> Integer -> Integer
testAll propLijst mutn xLength = do
    killedAllMutants <- percentageOfKilledMutants propLijst mutn
    let hoi = take xLength $ iterate (\x -> if not killedAllMutants then x+1 else x+0) 0
    return hoi
-}

isKilled :: PropertyList -> Mutator -> Gen Bool
isKilled propLijst mutn = do
    tableNum <- choose(1, 50)
    killedList <- mutate mutn mProp multiplicationTable tableNum
    let reallyKilledAll = not $ all (==False) killedList
    return reallyKilledAll










































