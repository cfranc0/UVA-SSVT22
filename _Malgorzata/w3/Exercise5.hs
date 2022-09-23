module Exercise5 where

import SetOrd
import Lecture3
import Test.QuickCheck
import Data.List
import Test.QuickCheck (Positive, Property)

-- Time spent: 90minutes

sub :: Form -> Set Form
sub (Prop x) = Set [Prop x]
sub (Neg f) = unionSet (Set [Neg f]) (sub f)
sub f@(Cnj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Dsj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Impl f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Equiv f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)


-- The set of the formula consisting one atom is a set of this atom
propPropTest :: Int -> Bool
propPropTest x = sub (Prop x) == Set [Prop x]

--
-- Testing whether the sub-formulas for formula -p ^ -q is {p, Neg p, q, Neg q, -p Cnj -q}
propFormTest :: Int -> Int -> Property
propFormTest x y = (x >=0 && y >= 0) ==> set1 `subSet` set2 && set2 `subSet` set1
    where
    px = Prop x
    py = Prop y
    set1 = sub (Cnj [Neg px, Neg py])
    set2 =  Set (sort (nub [px, py, Neg px, Neg py, Cnj [Neg px, Neg py]]))


{- 5.1 
Time spent 2,5h 
The implementation is hard to prove. We would have to know all the
sub-formulas for the given formula. 
One way we could do it is to implement a generator that 
generates a formula, and takes track of the sub-formulas used to create that
formula. This generator can be made using recursive approach, but would require some 
constraints like a maximum depth of the recursion tree.
-}


nsub' :: Set Form -> Int -> Int
nsub' (Set []) n = n
nsub' (Set (x:xs)) n = nsub' (Set xs) (n+1)

nsub :: Form -> Int
nsub f = nsub' (sub f) 0


-- The number of subformulas of the formula consisting one atom equals 1
propPropTest' :: Int -> Bool
propPropTest' x = nsub (Prop x) == 1


-- Testing whether the number of sub-formulas for formula -p ^ -q is 5
propFormTest' :: Int -> Int -> Property
propFormTest' x y = (x >=0 && y >= 0) ==> nsub (Cnj [Neg px, Neg py]) == 5
    where
    px = Prop x
    py = Prop y



