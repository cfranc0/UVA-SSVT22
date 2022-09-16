import Data.List

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation a b = length a == length b && all (`elem` b) a

--property_reverse :: Eq a => [a] -> [a] -> Bool
--property_reverse a b = isPermutation a b && isPermutation (reverse a) b && isPermutation  a (reverse b) && isPermutation  (reverse a) (reverse b)

{- to recognize the permutation of the list, there are few conditions that needs to be met
both lists need to have the same lenght, the same elements, and the same sum.
However, if we can assume, that the lists do not contain duplicates, we don't need to check the sum.
-}

