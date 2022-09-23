-- Time spent: 90 minutes --

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

{-
	In order to test the parse function we need to verify that:
	- Valid inputs are parsed into formula objects
	- Invalid inputs are not parsed into valid formula objects
	- Valid inputs are parsed into the right formula objects

	The first thing we can do is pass to the parse function strings for which we
	already know the correct formula objects.
-}

testKnown :: String -> [Form] -> Bool
testKnown s f = parse s == f

testKnown_prop = testKnown "1" [Prop 1]
testKnown_neg  = testKnown "-1" [Neg (Prop 1)]
testKnown_cnj  = testKnown "*(1 2)" [Cnj [Prop 1, Prop 2]]
testKnown_dsj  = testKnown "+(1 2)" [Dsj [Prop 1, Prop 2]]
testKnown_impl = testKnown "(1==>2)" [(Prop 1) `Impl` (Prop 2)]
testKnown_equiv= testKnown "(1<=>2)" [(Prop 1) `Equiv` (Prop 2)]

testKnown_bad  = testKnown "+*(1 2)" []

{-
	This, obviously, doesn't take into account nested formulas or any other
	combination not included in the single tests performed above.
	To do this we could use a function that randomly generates formulas and,
	along with them, their string rapresentation. That way, we could have a
	known-to-be-correct formula that can be used to compare the parsed result.
-}

