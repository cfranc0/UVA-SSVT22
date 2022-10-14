module Exercise5 where

import Data.List
import Test.QuickCheck
import SetOrd
import System.IO
import System.Random

-- Time spent: XX minutes -- started: 15:15 finished: xx:xx

{-
Use the datatype for relations from the previous exercise, plus

trClos ::  Ord a => Rel a -> Rel a 
that gives the transitive closure of a relation, represented as an ordered list of pairs. E.g., 
trClos [(1,2),(2,3),(3,4)] should give [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)].

-}

a = [(1,2),(3,4),(5,6),(7,8),(4,1)]
b = [(3,1),(5,2),(6,4),(1,3),(5,3)]

type Rel a = [(a,a)]

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- trClos [(1,2),(2,3),(3,4)]
--        [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]

-- trClos ::  Ord a => Rel a -> Rel a 
trClos l =  map (fst l @@ snd l) l

