--time spent 300 minutes


module Exercise4
where 
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
               -- where  input = n `div` toInteger (length mutators)



--take all cases : whether its survivor or not
countAll :: Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> ([Integer] -> Gen [Integer]) -> Gen Int
--countAll n props fun mutator = length . filter ( \n -> n == False || True) <$> mapM (survivor fun props mutator) [1 .. n]
countAll n props fun mutator = length  <$> mapM (survivor fun props mutator) [1 .. n]


--example : strength 14 [prop_tenElements, prop_firstElementIsInput] multiplicationTable mutators
--we calculate the percentage of strength dividing the number of the survivors with all the cases
strength :: Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> [([Integer] -> Gen [Integer])]-> IO Float
strength n props fun mutators = do 
                 {
                survivors<- generate $ countSurvivors n props fun mutators ;
                --we take the survivors and mutants in format we can use to calculate our percentage
                mutantsNumber<- generate $ countEverything n props fun mutators;
                --j<- generate $ countEverything (n `div` toInteger (length mutators)) props fun mutators;
                return (((fromIntegral (mutantsNumber - survivors)*100 / fromIntegral mutantsNumber)))} 



--testing the function was implemented by running some examples and because they sometimes give different results from the generator , we repeated them to see their behaviour 
--for example running generate $ sequence $ take 20 $ repeat $ countSurvivors 5 [prop_tenElements, prop_firstElementIsInput, prop_sumIsTriangleNumberTimesInput, prop_linear, prop_moduloIsZero] multiplicationTable mutators 
-- gvives [0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0] , which means that in general we either get one or zero survivors
--So for example here, generally it gives 1 survivor, so 100% or 1 survivor which is ((15-1)*100)/15=93,333 ,for input n=5 we use 15 because we have 3 mutators in our example
-- then in order to test our implementation we ran our strength function with the following command :
-- sequence $ take 20 $ repeat $ strength 5 [prop_tenElements, prop_firstElementIsInput, prop_sumIsTriangleNumberTimesInput, prop_linear, prop_moduloIsZero] multiplicationTable mutators
-- which outputs [100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,93.333336,100.0,100.0,100.0]