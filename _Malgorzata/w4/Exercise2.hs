module Exercise2 where

import LTS
import Exercise1
import Test.QuickCheck

-- Time spent: 300 minutes


{-
The more values we put in xs and ls the better, because the IOLTS will be more random, 
so it will test the function form exercise1 better.
-}
xs = [1 .. 9]
ls = ["coin", "button","coffee", "tea", "lemonade","card","book","beer","cheque"]


{-
I'm creating the cartesian products from all states and labels to get all possible transitions
-}
cartProd :: [a] -> [b] -> [a] -> [(a, b, a)]
cartProd xs ys zs = [(x,y,z) | x <- xs, y <- ys, z <- zs]


{-
Here I'm generating random sublists of all possible transitions
-}
genIOLTS :: [State] -> [Label] -> Gen IOLTS
genIOLTS vs ls = fmap createIOLTS (sublistOf (cartProd vs ls vs))

test :: Property
test = forAll (genIOLTS xs ls) $ \x -> validateLTS x

{-
At the begging, I was trying to generate states as random list of ints, and labels as random list of strings,
however i had a big problem with working with Gen types.
Than i decided to define some data, create all posibilities within it and randomly choose lists of transitions.
This way I added some randomness to the solution.
-}
















-- genInt' :: Gen Integer
-- genInt' = choose (0,9)

-- --genInt = sized genStates

-- genStates :: Int -> [Gen Integer]
-- genStates 0 = [choose (0,9)]
-- genStates n | n > 0 = choose (0,9) : genStates (n -1)
 



-- genChar :: Gen Char
-- genChar = choose ('a','z')



-- ltsGen :: Gen IOLTS
-- ltsGen = ([x],[a],[b], [(x,a,z),x])
--     where 
--         x = genInt
--         a = genChar
--         b = genChar
--         z = genInt


-- instance Arbitrary Form where
--     arbitrary :: Gen Form
--     arbitrary = Prop <$> choose (0,9)

-- genForm = sized genForm'

-- generator for formulas
-- genIOLTS :: [Gen Integer]
-- genIOLTS = [genInt]