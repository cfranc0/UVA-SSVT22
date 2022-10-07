module Exercise3 where
import Exercise2
import MultiplicationTable
import Test.QuickCheck
import Data.List
import Exercise4
import Mutation
import Data.Text.Internal.Read (digitToInt)
import CoreMonad (liftIOWithCount)





--minimal :: Integer -> [[[Integer] -> Integer -> Bool]] -> (Integer -> [Integer]) -> [([Integer] -> Gen [Integer])] -> Gen Int
--minimal n (props: ptopps) fun mutators = sum <$> mapM (countSurvivors n props fun ) mutators

--We try to take all the possible sets of properties and their names ( We also checked that all the possible sets are in the same order as the sets of their names)
--So we tried to extract for each one of the properties set their strength


--function to calculate all possible sets of properties
properties_sub ::  [[Integer] -> Integer -> Bool]-> [[[Integer] -> Integer -> Bool]]
properties_sub [] = [[]]
properties_sub (x:xs) = properties_sub xs ++ map (x:) (properties_sub xs)


--function to calculate all possible sets of properties names

properties_names_sub :: [[Char]]-> [[[Char]]]
properties_names_sub [] = [[]]
properties_names_sub (x:xs) = properties_names_sub xs ++ map (x:) (properties_names_sub xs)




--list of properties names
properties_names = ["prop_tenElements", "prop_firstElementIsInput", "prop_sumIsTriangleNumberTimesInput", "prop_moduloIsZero", "prop_linear"]

--list of actual properties
properties=[prop_tenElements, prop_firstElementIsInput, prop_sumIsTriangleNumberTimesInput, prop_linear, prop_moduloIsZero]

--generation of all possible properties sets 
properties_set=properties_sub properties

--generation of all possible properties sets 
properties_names_set=properties_names_sub properties_names





--In this function we throught all the sets of properties to calculate the strength of each one of them 
--we check for each strength if it is 100, in order to be eligible for minimal property set 
--Then we would take the index n in order to find the actual set of properties from the properties_names_set list

--   loop index -> all subset of properties -> IO result
minimal ::  Int->[[[Integer] -> Integer -> Bool]]  -> IO Int
minimal n [[]]=return (-1)
minimal _ [] =return (-1)
minimal n (p:ps) = do  {

--we check for each property set p , the strengt value 
            y <-  strength 4000 p multiplicationTable mutators ;

--also tried to extract the properties names from here, but it didint work
            --if y==0 then return (intercalate "," x) else  minimal ps xs 
            --if y==100 then return (properties!! (n )) else minimal (n+1) ps  ;
        --check for each one of the strengths if it 100 and return the index , otherwise go to the next property set
            if y==100 then  return n   else minimal (n+1) ps  ;
        }   
        --minimal (n+1) ps
--minimal (p:ps) (x:xs) = x 


--Also tried with lamdas and do blocks inside them, but again didnt work 

--res= (\a b->  do {
--    
--               y <- generate $ countSurvivors 4 [prop_tenElements, prop_firstElementIsInput, prop_sumIsTriangleNumberTimesInput] multiplicationTable [addElements];
                
                --if g==0 then print b else print "";
--                return () ) (x=properties_sub properties) (properties_names_sub properties)


