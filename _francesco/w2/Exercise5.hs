-- Time spent: 1.5h

import Data.List
import Test.QuickCheck

-- Inspiration from https://stackoverflow.com/a/16379034/9754741
rotate :: Int -> [Int] -> [Int]
rotate by xs = take (length xs) $ drop by (cycle xs)

{-
    The deran function takes an integer input and creates all derangements of
    the list [0..n-1], which are all the permutations of [0..n-1] that do not
    have any item in the same position as the original list.
    Tho generate them it's quite esay, as it's simply all the lists rotate by 1
    to n-1 steps to the right (or left).

    A list with n items, ranging from 0 to n-1, has, therefore, n-1 derangements
    ranging from 1 to n-1 rotation steps.
-}
deran:: Int -> [[Int]]
deran n
    | n <= 1    = []
    | otherwise = map (\s -> rotate s [0..(n-1)]) [1..n-1]

{-
    For a lists to be derangements of each other we need to verify that:
    - They are of the same length
    - All elements of one list must be present in the other, and vice-versa
    - Each element does not appear in the same position in both lists
-}
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement a b = length a == length b && all (`elem` b) a && all (`elem` a) b && all (\i -> not $ a!!i == b!!i) [0..(length a)-1]

{-
    The differentLenght prop checks whether two lists of different sizes are
    correctly detected as not derangements.
    In the case that quickCheck generates an empty list, we chech that it is,
    in this case, detected as a derangement. I didnt let quickCheck generate two
    different lists are there are no guarantees that they are in fact not
    derangements of each other since they are generate at random.
-}
prop_differentLenght :: [Int] -> Bool
prop_differentLenght xs = case xs of
    []     -> isDerangement xs xs
    (x:xs) -> not $ isDerangement xs (x:xs)

{-
    The missingItem props checks that a list is not considered a derangement of
    another if there are items not in common between them, even though the same
    position in both lists has different items.
    This is done on both the right and left argument of the function.
-}

missingItems = [[1,2,3,5], [1,1,2,3]]

prop_missingItemsLeft :: Bool
prop_missingItemsLeft = not $ all (\xs -> all (\xs' -> isDerangement xs' [1..4]) (permutations xs)) missingItems

prop_missingItemsRight :: Bool
prop_missingItemsRight = not $ all (\xs -> all (isDerangement [1..4]) (permutations xs)) missingItems

{-
    The permutation operation always returns 
-}
factorial :: Int -> Int
factorial n = foldl (*) 1 n

--- Not working
prop_permutations :: NonEmptyList Int -> Bool
prop_permutations (NonEmpty xs) = all (\i -> if (i `mod` (length xs)) then (not $ isDerangement xs (permutations xs !! i)) else isDerangement xs (permutations xs !! i)) ([0..(factorial $ length xs) - 1])