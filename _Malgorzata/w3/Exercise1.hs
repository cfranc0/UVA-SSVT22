
module Exercise1 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Test.QuickCheck.Property (Prop)


contradiction :: Form -> Bool
contradiction f = all (\x -> not (evl x f)) (allVals f)

tautology :: Form -> Bool
tautology f = all (\x -> evl x f) (allVals f)

entails :: Form -> Form -> Bool
entails p q = all (\x -> evl x (Impl p q)) (allVals q)

equiv :: Form -> Form -> Bool
equiv p q = all (\x -> evl x (Equiv p q)) (allVals p)

{- I would implement some formulas that i know they are: contradictions, tautologies (from truth table)
and test them against the contradiction and tautology function.
Testing formula that is tautology against tautology function should return true, and false 
for contradiction function.  
Testing formula that is a contradiction against contradiction function should return true,
and false for tautology function.  
Then I would implement formula that implificates another formula and check it against
entails function. 
For testing equiv function we can compare the formula to itselfes and in 
second test to different one that isin't equivalent in truth table.
-}