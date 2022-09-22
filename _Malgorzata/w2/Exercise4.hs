-- Time spent: 4h
module Exercise4 where
import Data.List
import Test.QuickCheck

{-
    The isPermutation function makes sure the two lists are a permutation of
    each other by leveraging the fact that they don't allow duplicates.
    The first thing we do is make sure the lenght is the same. Secondly, we
    check that the elements in one list are also in the other. We don't need to
    do this both times for the two lists, as the absence of duplicates insures
    that a missing number would be always detect as the number that replaced it
    missing.
-}
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation a b = length a == length b && all (`elem` b) a

{-
    The properties tested for the permutation are:
    - Making sure the permutation of an empty list, which is itself, is still
      correctly detected by the isPermutation function
    - Checking that a permutation of a list with only one item, which is itself,
      is correctly detected by the isPermutation function
    - Checking that two lists of different length are immediately detected as
      not being sublists
    - Insuring that two lists generated in a way that makes them permutations of
      each other are correctly detected as permutations
-}
emptyList = [] :: [Int]

prop_length0 :: Bool
prop_length0 = isPermutation emptyList emptyList

prop_length1 :: Int -> Bool
prop_length1 n = isPermutation [n] [n]

prop_differentLenth :: [Int] -> Bool
prop_differentLenth xs = case xs of
    []     -> isPermutation xs xs
    (x:xs) -> not $ isPermutation xs (x : xs)

{-
    This next property is trying to verify that the function does return that a
    list is a permutation if we know that it is.
    
    The first implementation that I created was this one:
    prop_validPermutation xs = all (isPermutation xs) (permutations xs)

    I mean, it does work.. It's just sooo incredibly compute intensive 🙈, as it
    check that any permutation of any list is correctly detected by my function,
    which can become a bit of a struggle when we start to consider lists with
    more than a 4/5 (with 5 we are already at 5! = 120 possibilities).

    The new approach utilizes simply makes sure that the list generate by
    quickCheck is actually not empty, since we are already testing that, and it
    pickes only up to 8 items.
-}

prop_validPermutation :: NonEmptyList Int -> Bool
prop_validPermutation (NonEmpty xs) = all (isPermutation (take 8 xs)) (permutations (take 8 xs))
    
{-
    I would rate the properties from the weakest to the stringest as:
    1. prop_validPermutation and prop_differentLenght since whatever list is
       allowed
    2. prop_length1 since whatever integer, inside a list, is valid
    3. prop_length0 since there is only one possible valid choice

    The testing process can be automated, utilizing quickCheck and the provided
    properties.
-}
