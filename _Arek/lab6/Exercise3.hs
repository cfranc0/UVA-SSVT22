module Exercise3 where

import Data.List

import SetOrd

--import System.Random

-- Time spent: 30 minutes -- started: 14:30 finished: xx

{-
Suppose we implement binary relations as list of pairs, Haskell type [(a,a)]. 

Assume the following definition:
> type Rel a = [(a,a)]
Use the following declaration:

symClos :: Ord a => Rel a -> Rel a
    
to define a function that gives the symmetric closure of a relation, 
where the relation is represented as an ordered list of pairs. E.g., 
symClos [(1,2),(2,3),(3,4)] should give [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)].
-}

--symClos :: Ord a => Rel a -> Rel a