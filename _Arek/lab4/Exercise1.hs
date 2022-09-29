module Exercise1 where

import Data.List
import LTS    
-- import Test.QuickCheck

-- Time spent: x minutes -- start 21:25

-- The IOLTS datatype allows, by definition, for the creation of IOLTS's that are not valid. 
-- Make a list of factors that result in invalid IOLTS's. Write a function validateLTS :: IOLTS -> Bool 
-- that returns true iff a given LTS is valid according to the definition given in the Tretmans paper.

-- Definition 6. A labelled transition system with inputs and outputs is a 5-tuple
--   Q, Li , Lu , T, q0〉 where
--   Q, Li ∪ Lu , T, q0〉 is a labelled transition system in LTS(Li ∪ Lu);
--   LI and LU are countable sets of input labels and output labels, respectively, which are disjoint: Li ∩ Lu = ∅.

-- List of factors that result in invalid IOLTS:
--   Li and Lu must not have any common elements

-- To test, we can generate IOLTS which should break the rules above and result in FALSE
-- createIOLTS [(0, "?but", 1), (1, "?liq", 1), (1, "!liq", 2), (2, "?liq", 2)] -> ([0,1,2],["but","liq"],["liq"],[(0,"but",1),(1,"liq",2),(1,"liq",1),(2,"liq",2)],0)
-- ["but","liq"],["liq"] liq is repeated, therefore the IOLTS is invalid

duplicates :: Eq a => [a] -> [a] -> Bool
duplicates a b = length (nub l) /= length l
    where   
        l = a ++ b  

validateLTS :: IOLTS -> Bool
validateLTS (a, b, c, d, e) 
    | duplicates b c = False
    | otherwise = True
