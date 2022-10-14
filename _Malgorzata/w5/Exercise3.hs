module Exercise3 where
import Exercise2
import MultiplicationTable
import Mutation
import Test.QuickCheck

-- time spent 300mins

--import Control.Monad.Cont (liftM)

--powerSet :: [a] -> [[a]]
powerSet [] = [[]]
powerSet (x:xs) =  ([x:y | y<-powerSet xs] ++ powerSet xs)

powerSet2 :: Monad m => [a] -> m [[a]]
powerSet2 f@(x:xs) =
    do
        if null f
            then return [[]]
        else
            return ([x:y | y<-powerSet xs] ++ powerSet xs)

type LabeledProps = (Integer, [Integer] -> Integer -> Bool)

type SurvivalCount = Int


propL = [(1,prop_tenElements), (2, prop_firstElementIsInput), (3, prop_sumIsTriangleNumberTimesInput),(4, prop_linear),(5, prop_moduloIsZero)]

propS = ["prop_tenElements", "prop_firstElementIsInput", "prop_sumIsTriangleNumberTimesInput", "prop_linear", "prop_moduloIsZero"]

{-
Here is the failed approach. I have created a poweSet function that returns powerset of properties.
I wanted to go over each set and return the number of surviors for this set.
Then i would check which one has the least number of properties with 0 survivors and that would be the answer.
I was able to get the results, but i couldn't return it with the set of properties for this result.
Since the powerSet always returns sets in the same order i could compare it manualy.
The result is property 1 and 4.
I couldn't make it work because i was strugling with the Gen type.
-}


            --     fut                        props          n               mutators
--minimalSet :: (Integer -> [Integer]) -> [LabeledProps] -> Integer -> [[Integer] -> Gen [Integer]] -> [Gen Int]
--minimalSet fut props n mutators =  filter (>0)  <$> [countSurvivors n [x] fut mutators |[(y,x)] <- powerSet propL]
--minimalSet fut props n mutators =  filter (\[(x,y)] -> forAll (countSurvivors n y fut mutators) $ \t  -> t == 0) (powerSet2 props)



--getSurvivors :: Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> [[Integer] -> Gen [Integer]]-> Maybe [[Integer] -> Integer -> Bool]
--getSurvivors n props fut mutators = do
--     subProps <- powerSet2 props
--     if survivors == 0
--         then return Just props
--     else
--         return []


{-
Another approach was to create 3D matrices with the result for every input, mutators, and set of property.
Then i would look for the combination that leads to the value False for at least each mutator.
I was able to implement 3D matrix, because i run out of the time.
-}


-- allSets n fun mutators = fmap (forMutators n fun mutators) (powerSet multiplicationTableProps)

-- forMutators :: Integer -> (Integer -> [Integer]) -> [([Integer] -> Gen [Integer])] -> [[Integer] -> Integer -> Bool] -> Gen [[[Bool]]]
-- forMutators n fun mutators props = mapM (forInputs input props fun ) mutators
--             where input = n `div` toInteger (length mutators)


-- forInputs :: Integer -> [[Integer] -> Integer -> Bool]-> (Integer -> [Integer])-> ([Integer] -> Gen [Integer])-> Gen [[Bool]]
-- forInputs n props fut mutator =  mapM (oneMutant fut props mutator) [1 .. n]

-- oneMutant :: (Integer -> [Integer]) -> [[Integer] -> Integer -> Bool] -> ([Integer] -> Gen [Integer]) -> Integer -> Gen [Bool]
-- oneMutant fut props mutator input =  mutate' mutator props fut input