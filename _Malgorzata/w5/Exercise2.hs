module Exercise2 where
import MultiplicationTable
import Test.QuickCheck
import Data.List
import Mutation
import Test.QuickCheck (Arbitrary(arbitrary), shuffle)
import Mutation (removeElements, shuffleList)



--             n of mutants          list of properties               function under test        mutators               n of surviving mutants

--countSurvivors :: Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> ([Integer] -> Gen [Integer]) -> Gen [Bool]
countSurvivors n props fun mutators =  [countS n props fun x | x <- mutators]
--countSurvivors n props fun mutator = mapM (survivor fun props mutator) [1 .. n]



-- This functions counts how many mutants survived for the n number of inputs.

--countS :: Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> ([Integer] -> Gen [Integer]) -> Gen [Bool]
countS n props fun mutator = length <$> filter (== False ) <$> mapM (survivor fun props mutator) [1 .. n]
    
    
-- mapM (countSurvivors n [prop_tenElements, prop_firstElementIsInput, prop_sumIsTriangleNumberTimesInput] multiplicationTable) [addElements, removeElements, shuffleList]




--The survivor is the mutant that doesn't get killed by any property for the given input
-- so if the mutate' returns table [False, False, False, False] it means that the mutant wasn't killed

--                 fun                         list of props            mutator                       input     killed or not       
survivor :: (Integer -> [Integer]) -> [[Integer] -> Integer -> Bool] -> ([Integer] -> Gen [Integer]) -> Integer -> Gen Bool
survivor fun props mutator input =  all (== False) <$> mutate' mutator props fun input


