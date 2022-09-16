import Data.List
import Test.QuickCheck
import Test.QuickCheck.Test (test)

-- to recognize the permutation of the list, there are few conditions that needs to be met
--both lists need to have the same lenght, and they shoudl have the same elements
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation a b = length a == length b && all (`elem` b) a && all (`elem` a) b

-- Lists are the same size
propertyLength :: Eq a => [a] -> [a] -> Bool
propertyLength a b = length a == length b

-- All elements of a are also in the second list (b)
propertyElemsOne :: Eq a => [a] -> [a] -> Bool
propertyElemsOne a = all (`elem` a)

-- All elements of b are also in the second list (a)
-- It is important to check it both ways a in b and b in a, in case they are different length
propertyElemsTwo :: Eq a => [a] -> [a] -> Bool
propertyElemsTwo b = all (`elem` b)


testWithOutput :: [Int] -> [Int] -> IO ()
testWithOutput a b = putStrLn ("Testing " ++ show a ++ " and " ++ show b ++ ": " ++ show (isPermutation a b))

-- if we can assume there are no duplicates, there is less cases to check,
-- and we won't know whether the function will work with the duplicates.
isPermutationTest = do                                    -- results :
    testWithOutput [1,2,3] [1,2,3,4] -- different lengths    False
    testWithOutput [1,2,3,4] [1,2,3]                      -- False
    testWithOutput [1,2,3] [1,2,3] -- correct case           True
    testWithOutput [] [1,2,3] -- for empty list              False
    testWithOutput [] []                                  -- True


-- We don't know how to provide an ordered list of properties by strenght
--The properties are hard do compare.
