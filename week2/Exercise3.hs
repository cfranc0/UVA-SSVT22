-- Time spent: 2h

import Data.List
import Text.Show.Functions

-- From lab1
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- From lab2
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

-- -- --

ex1L, ex1R :: Int -> Bool
ex1L n = even n && n > 3
ex1R = even

ex2L, ex2R :: Int -> Bool
ex2L n = even n || n > 3
ex2R = even

ex3L, ex3R :: Int -> Bool
ex3L n = (even n && n > 3) || even n
ex3R = even

ex4L, ex4R :: Int -> Bool
ex4L = even
ex4R n = (even n && n > 3) || even n

{-
    This funciton returns the comparison of two conditions in a given domain.
-}
strengthChecker :: [a] -> (a -> Bool) -> (a -> Bool) -> Ordering
strengthChecker xs p q = case stronger xs p q of
    (True) -> if (weaker xs p q) then EQ else GT
    (False) -> LT

{-
    We can now test all the different exercises to make sure we got the right
    result in the workshop. At the same time, this does not help in creating the
    list of strength...
-}
ex1 = strengthChecker [(-10)..10] ex1L ex1R
ex2 = strengthChecker [(-10)..10] ex2L ex2R
ex3 = strengthChecker [(-10)..10] ex3L ex3R
ex4 = strengthChecker [(-10)..10] ex4L ex4R

{-
    This was my first solution into creating the ordered list of strength.
    The problem with this implementation is that the output from ghci only
    returns [<function>, ...] which is totally useless in understanding what the
    result of the function is.
-}
sorted :: [Int -> Bool]
sorted = reverse $ sortBy (\l r -> strengthChecker [(-10)..10] l r) [ex1L, ex1R, ex2L, ex2R, ex3L, ex3R, ex4L, ex4R]

{-
    There we go! 🥳
    By changing the functions to (function, label) pairs there now is
    human-readable information on what the order is.
    
    The result is:
    [(<function>,"1L"),(<function>,"4R"),(<function>,"4L"),(<function>,"3R"),(<function>,"3L"),(<function>,"2R"),(<function>,"1R"),(<function>,"2L")]

    Is this correct?

    - 2L is the weakest condition in this exercise, as it will allow all even
      numbers along side every number grater than 3. In our domain [-10, 10],
      this results in 14 numbers considered valid, compared to the 11 for
      just 'even'.
    - 1R, 2R, 3R and 4L are all equal to 'even', along with 3L and 4R, which,
      despite beign seamingly different are actually equivalent. Because of
      this, the order in which they appear in the final list is indifferent.
    - The only other condition left is 1L, which only accepts even values which
      are also greater than 3, resulting in only 4 valid numbers in our domain.

    The list returned by the function is, therefore, correct.
-}
sortedWithLabels :: [(Int -> Bool, [Char])]
sortedWithLabels = reverse $ sortBy (\(l, _) (r, _) -> strengthChecker [(-10)..10] l r) [(ex1L, "1L"), (ex1R, "1R"), (ex2L, "2L"), (ex2R, "2R"), (ex3L, "3L"), (ex3R, "3R"), (ex4L, "4L"), (ex4R, "4R")]