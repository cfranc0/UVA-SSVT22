import MultiplicationTable
import Test.QuickCheck
import Data.List
import Mutation
import Exercise2
import Data.Text.Internal.Read (digitToInt)


--since we have calculated the survivors from exercise 2, we need to find the number of all cases (survivor or not) with all the mutators and divide the survivors with that number 



--count the number of all cases 
countEverything :: Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> [([Integer] -> Gen [Integer])] -> Gen Int
countEverything n props fun mutator = sum <$> mapM (countAll n props fun ) mutators

-- generate $ countSurvivors 4 [prop_tenElements, prop_firstElementIsInput, prop_sumIsTriangleNumberTimesInput] multiplicationTable [addElements]


--take all cases : whether its survivor or not
countAll :: Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> ([Integer] -> Gen [Integer]) -> Gen Int
--countAll n props fun mutator = length . filter ( \n -> n == False || True) <$> mapM (survivor fun props mutator) [1 .. n]
countAll n props fun mutator = length  <$> mapM (survivor fun props mutator) [1 .. n]


--example : strength 14 [prop_tenElements, prop_firstElementIsInput] multiplicationTable mutators
--we calculate the percentage of strength dividing the number of the survivors with all the cases
strength :: Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> [([Integer] -> Gen [Integer])]-> IO Float
strength n props fun mutators = do 
                 {
                t<- generate $ countSurvivors n props fun mutators ;
                j<- generate $ countEverything n props fun mutators;
                return (fromIntegral t/ fromIntegral j)}
                    
