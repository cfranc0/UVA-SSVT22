import Test.QuickCheck
import Data.List
import MultiplicationTable
import Data.Maybe
import Debug.Trace

--reverse all items in order to see how the property prop_firstElementIsInput would react 
reversemut' :: [Integer] -> Gen [Integer]
reversemut' []= return []
reversemut' xs = return $  reverse xs




--return and empty list to see how  prop_firstElementIsInput, properties  would react 
giveempty :: [Integer] -> Gen [Integer]
giveempty x = return []

--here we give only the first input as result 
giveitem :: [Integer] -> Gen [Integer]
giveitem (x:xs) = return [x]

--here we negate all the items to check how property prop_sumIsTriangleNumberTimesInput
negative :: [Integer] ->Gen [Integer]
negative xs = return  (map (*(-1)) xs)