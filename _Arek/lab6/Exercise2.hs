module Exercise2 where

import Data.List
import Test.QuickCheck
import SetOrd
import System.IO
import System.Random
import Exercise1

-- Time spent: XXX minutes -- started: 13:30

{-
--Implement operations for set intersection, set union and set difference, for the datatype Set defined in SetOrd.hs. 
Next, use automated testing to check that your implementation is correct. 
First use your own generator, next use QuickCheck.
--Use the following declarations:

setIntersection :: Ord a => Set a -> Set a -> Set a
setUnion :: Ord a => Set a -> Set a -> Set a
setDifference :: Ord a => Set a -> Set a -> Set a

-}

a = list2set [1,2,3,6,10]
b = list2set [1,4,7,10]

setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection (Set s1) (Set s2) = Set $ intersect s1 s2

setUnion :: Ord a => Set a -> Set a -> Set a
setUnion (Set s1) (Set s2) = Set $ union s1 s2

setDifference :: Ord a => Set a -> Set a -> Set a
setDifference (Set s1) (Set s2) = Set $ s1 \\ s2

-- generate $ setIntersectionTest (genNumsQC 10) (genNumsQC 10)
