import Data.List
import Test.QuickCheck

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concatMap (insrt x) (perms xs) where
    insrt x [] = [[x]]
    insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

{-
source http://geekyplatypus.com/generating-permutations-and-derangements-using-haskell/
This function wasn't completly correct - it wasn't working for lists that are different sizes.
So we added a condition that checks it.
-}
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement xs ys = (length xs == length ys) && and [ x `elem` ys && (index x xs /= index x ys) | x <- xs ] where
      index n (x:xs) | n == x = 0
                     | otherwise = 1 + index n xs

-- we create permutations of the list, but we only take lists that are derangements
deran :: Int -> [[Int]]
deran n = filter (isDerangement [0 .. (n-1)]) (perms [0 .. (n-1)])

-- should have the same elements
propertyElems :: Bool
propertyElems = isDerangement [0] [1]

-- should be equal size
propertyDifferentLength :: Bool
propertyDifferentLength = isDerangement [0,1] [1]

-- if a is derangement of b then b is darangement of a
propertySymetric :: Bool
propertySymetric = isDerangement [0,1] [1,0] && isDerangement [1,0] [0,1]

testWithOutput' :: [Int] -> [Int] -> IO ()
testWithOutput' a b = putStrLn ("Testing " ++ show a ++ " and " ++ show b ++ ": " ++ show (isDerangement a b))

-- test report
isDerangementTest :: IO ()
isDerangementTest = do                                    -- results :
    testWithOutput' [1,2,3] [1,2,3,4] -- different lengths    False
    testWithOutput' [1,2,3,4] [1,2,3]                      -- False
    testWithOutput' [1,2,3] [3,1,2] -- correct case           True
    testWithOutput' [1,1,2] [1,2,3] --duplicates              False
    testWithOutput' [] [1,2,3] -- for empty list              False
    testWithOutput' [] []                                  -- True


--the solution for the ordering properties was borrowed from the Exercise 3.
stronger :: (Bool) -> (Bool) -> Bool
stronger p q = p --> q

strengthChecker :: (Bool) ->  (Bool) -> Ordering
strengthChecker p q = if stronger p q then (if stronger q p then EQ else GT) else LT


sortedWithLabels :: [(Bool, [Char])]
sortedWithLabels = reverse $ sortBy (\(l, _) (r, _) -> strengthChecker l r) [(propertyElems, "propertyElems"), (propertyDifferentLength, "propertyDifferentLength"), (propertySymetric, "propertySymetric")]


-- it turned out that the weakest property is propertyDifferentLength then propertyElems and propertySymetric.
-- which actually seems right. If we have different lenghts of lists, there is no way that it could be a derangement,
-- however, if the lists are symetric, they must have the same length, elements, and they are corretly build.


-- here is a failed attempt to automate the process
-- source https://stackoverflow.com/questions/16440208/how-to-generate-arbitrary-instances-of-a-simple-type-for-quickcheck
-- https://www.stackbuilders.com/blog/a-quickcheck-tutorial-generators/
-- data GenLists = GenLists [Int] deriving (Show)
-- instance Arbitrary GenLists where
--     arbitrary = GenLists <$> sublistOf [0..9]


-- time spent 3h