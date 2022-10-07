import Test.QuickCheck
import Data.List
import MultiplicationTable
import Data.Maybe
import Debug.Trace


reversemut' :: [Integer] -> Gen [Integer]
reversemut' []= return []
reversemut' xs = return $  reverse xs

simplefun :: [Integer] -> [Integer]
simplefun []=[]
simplefun (x:xs) = simplefun xs ++ [x]


giveempty :: [Integer] -> Gen [Integer]
giveempty x = return []

giveitem :: [Integer] -> Gen [Integer]
giveitem (x:xs) = return [x]


negative :: [Integer] ->Gen [Integer]
negative xs = return  (map (*(-1)) xs)