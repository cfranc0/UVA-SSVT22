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

propertyNotEqual :: Eq a => [a] -> [a] -> Bool
propertyNotEqual xs ys = and [ x `elem` ys && (index x xs /= index x ys) | x <- xs ] where
      index n (x:xs) | n == x = 0
                     | otherwise = 1 + index n xs


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


-- here is a failed attempt to automate the process
-- source https://stackoverflow.com/questions/16440208/how-to-generate-arbitrary-instances-of-a-simple-type-for-quickcheck
-- https://www.stackbuilders.com/blog/a-quickcheck-tutorial-generators/
-- data GenLists = GenLists [Int] deriving (Show)
-- instance Arbitrary GenLists where
--     arbitrary = GenLists <$> sublistOf [0..9]


-- here is also a failed attempt to provide an ordered list of properties by strength using the weaker and stronger definitions.
stronger :: [a] -> [a] -> ([a] -> [a] -> Bool) -> ([a] -> [a] -> Bool) -> Bool
stronger xs ys p q = p xs ys --> q xs ys

strengthChecker :: [a] -> [a] -> ([a] -> [a] -> Bool) -> ([a] -> [a] -> Bool) -> Ordering
strengthChecker xs ys p q = if stronger xs ys p q then (if stronger xs ys q p then EQ else GT) else LT


sortedWithLabels :: [(Int -> Bool, [Char])]
sortedWithLabels = reverse $ sortBy (\(l, _) (r, _) -> strengthChecker [1,2,3,4] [1,2,3,4] l r) [(propertyEqual, "1L"), (propertyLength, "1R"), (propertyElemsOne, "2L")]

-- time spent 3h