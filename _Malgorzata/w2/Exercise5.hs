import Data.List
import Test.QuickCheck

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concatMap (insrt x) (perms xs) where
    insrt x [] = [[x]]
    insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)


{- source http://geekyplatypus.com/generating-permutations-and-derangements-using-haskell/
we chose this function because it's correct. Firstly, it checks if element of one list is an element
of second list, then it checks if they are not on the same place for example [1,2,3] and [3,2,1]. 
-}
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement xs ys = and [ x `elem` ys && (index x xs /= index x ys) | x <- xs ] where
      index n (x:xs) | n == x = 0
                     | otherwise = 1 + index n xs


-- we create permutations of the list, but we only take lists that are derangements
deran :: Int -> [[Int]]
deran n = filter (isDerangement [1 .. (n-1)]) (perms [0 .. (n-1)])

