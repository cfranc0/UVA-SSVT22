import Data.List
import Test.QuickCheck
import Exercise4

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concatMap (insrt x) (perms xs) where
    insrt x [] = [[x]]
    insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)


{-
source http://geekyplatypus.com/generating-permutations-and-derangements-using-haskell/
This function wasn't completly correct - it wasn't working for lists that are different sizes.
So we added a condition that checks it.
-}
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement xs ys = length xs == length ys && and [ x `elem` ys && (index x xs /= index x ys) | x <- xs ] where
      index n (x:xs) | n == x = 0
                     | otherwise = 1 + index n xs


-- we create permutations of the list, but we only take lists that are derangements
deran :: Int -> [[Int]]
deran n = filter (isDerangement [1 .. (n-1)]) (perms [0 .. (n-1)])

{-
We can use the properties from the exercise 4, because derangements are permutations
with additional property: ,,A derangement of the list [0..n-1] of natural numbers 
is a permutation π of the list with the property that for no x in the list π(x)=x"
-}

propertyEqual :: Eq a => [a] -> [a] -> Bool
propertyEqual xs ys = and [ x `elem` ys && (index x xs /= index x ys) | x <- xs ] where
      index n (x:xs) | n == x = 0
                     | otherwise = 1 + index n xs
