module Exercise2 where

import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable
import FitSpec

--countSurvivors :: Integer -> [([Integer] -> Integer -> Property)] -> (Integer -> [Integer]) -> Integer
countSurvivors nMutants lProp fut = generate $ mutate removeElements MultiplicationTable.prop_tenElements MultiplicationTable.multiplicationTable 1