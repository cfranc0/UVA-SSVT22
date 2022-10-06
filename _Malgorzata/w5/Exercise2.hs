module Exercise2 where
import MultiplicationTable
import Test.QuickCheck
import Data.List
import Mutation
import Data.Text.Internal.Read (digitToInt)


-- This function sums the number of all survived mutants from all mutators for all inputs and properties
--             n of mutants          list of properties               function under test        mutators               n of surviving mutants
countSurvivors :: Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> [([Integer] -> Gen [Integer])] -> Gen Int
countSurvivors n props fun mutator = sum <$> mapM (countS n props fun ) mutators

-- generate $ countSurvivors 4 [prop_tenElements, prop_firstElementIsInput, prop_sumIsTriangleNumberTimesInput] multiplicationTable [addElements]


-- This functions counts how many mutants survived for the n number of inputs for the given mutator and set of properties
countS :: Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> ([Integer] -> Gen [Integer]) -> Gen Int
countS n props fun mutator = length . filter (== False ) <$> mapM (survivor fun props mutator) [1 .. n]


--The survivor is the mutant that doesn't get killed by any property for the given input
-- so if the mutate' returns table [False, False, False, False] it means that the mutant wasn't killed

--                 fun                         list of props            mutator                       input     killed or not       
survivor :: (Integer -> [Integer]) -> [[Integer] -> Integer -> Bool] -> ([Integer] -> Gen [Integer]) -> Integer -> Gen Bool
survivor fun props mutator input =  all (== False) <$> mutate' mutator props fun input

-- maybe_divide  = do
--     a <- Just (countSurvivors 4 [prop_tenElements, prop_firstElementIsInput, prop_sumIsTriangleNumberTimesInput] multiplicationTable [addElements])
--     b <- 4
--     return (a/b)