
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

 --(\ x -> even x && x > 3) or even
fun1 :: Int -> Bool
fun1 x = even x && x > 3

funEven :: Int -> Bool
funEven = even

--(\ x -> even x || x > 3) or even
fun2 :: Int -> Bool
fun2 x = even x || x > 3

--(\ x -> (even x && x > 3) || even x) or even
fun3 :: Int -> Bool
fun3 x = (even x && x > 3) || even x

--even or (\ x -> (even x && x > 3) || even x)
fun4 :: Int -> Bool
fun4 x = ( even x && x > 3) || even x


(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)

weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
weaker xs p q = stronger xs q p

compareF :: (Int -> Bool) -> (Int -> Bool) -> Ordering
compareF f1 f2
    | weaker [-10 .. 10] f1 f2 = LT
    | stronger [-10 .. 10] f1 f2 = GT
    | otherwise = EQ


-- result :: [Int -> Bool]
result :: [Int -> Bool]
result = sortBy compareF [fun1, fun2, fun3, fun4, funEven]