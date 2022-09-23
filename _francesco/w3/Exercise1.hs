-- Time spent: 30 minutes --

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

-- Satisfiable is given in the lecture slides
satisfiable :: Form -> Bool
satisfiable f = any (\ v -> evl v f) (allVals f)

-- Contradiction is the oppostite of satisfiable
contradiction :: Form -> Bool
contradiction f = not (satiasfiable f)
    
-- A tautology is a formula which is satisfied by all valuations
-- This means that for all possibile valuations of the formula f, they have to
-- evaluate to true
tautology :: Form -> Bool
tautology f = all (\v -> evl v f) (allVals f)

-- | logical entailment 
{-
	From the lecture slides:
	B logically entails A is true if and only if all the valuations that satisfy
	B also satisfy A

	This means that we need to make sure that every valuations of A evals to
	true for A but also for B.
	This also means that A must always imply B: this is the tautology of A==>B
-}
entails :: Form -> Form -> Bool
entails a b = tautology (a `Impl` b)

-- | logical equivalence
{-
	For A to be equivalent to B, all valuations of A must be true for both A
	and B. This is also the tautology of A<=>B
-}
equiv :: Form -> Form -> Bool
equiv a b = tautology (Equiv a b)
