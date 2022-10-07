module Exercise2 where
import MultiplicationTable
import Test.QuickCheck
import Data.List
import Mutation
import Data.Text.Internal.Read (digitToInt)


-- This function sums the number of all survived mutants from all mutators for all inputs and properties
-- We need to divide the number of mutants by the number of mutators, because let's say n == 100, and we have 2 mutators.
-- So in the end we would have 200 mutants. There could be the case that we have more survivors than mutants.
--             n of mutants          list of properties               function under test        mutators               n of surviving mutants
countSurvivors :: Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> [([Integer] -> Gen [Integer])] -> Gen Int
countSurvivors n props fun mutators = sum <$> mapM (countS input props fun ) mutators
            where input = n `div` toInteger (length mutators)

-- This functions counts how many mutants survived for the n number of inputs for the given mutator and set of properties
--    n of mutants      lits of propr                     function under test            mutator
countS :: Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> ([Integer] -> Gen [Integer]) -> Gen Int
countS n props fut mutator = length . filter (== True ) <$> mapM (survivor fut props mutator) [1 .. n]


--The survivor is the mutant that doesn't get killed by any property for the given input
-- so if the mutate' returns table [False, False, False, False] it means that the mutant was killed
--                 function under test                         list of props            mutator         input     killed or not       
survivor :: (Integer -> [Integer]) -> [[Integer] -> Integer -> Bool] -> ([Integer] -> Gen [Integer]) -> Integer -> Gen Bool
survivor fut props mutator input =  all (== True) <$> mutate' mutator props fut input

-- generate $ countSurvivors 4 [prop_tenElements, prop_firstElementIsInput, prop_sumIsTriangleNumberTimesInput] multiplicationTable [addElements]