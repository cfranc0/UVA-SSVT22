module Exercise1 where
import Test.QuickCheck
import Mutation


-- we are the group of 3.


-- time spent: 45minutes

-- the output is list in reversed order
reverseList :: [Integer] -> Gen [Integer]
reverseList xs = do
      return $ reverse xs

-- the output is the list with reordered elements
-- sometimes shuffling can give the same list, thats why we
-- need to check if it's different the input
shuffleList :: [Integer] -> Gen [Integer]
shuffleList list = do
        l <- shuffle list
        if l == list then shuffleList list
        else return l

-- the output is the 2 cycles of the list
-- 
cycleList :: [Integer] -> Gen [Integer]
cycleList list = do
    return $ take 2 (cycle list)

-- the output will be the multiplied list by random elements
-- we know it always wil be different because we pick randomly number from 2-9
multiplyList :: [Integer] -> Gen [Integer]
multiplyList l = do
    m <- choose (2,9)
    return $ fmap (* m) l

mutators = [anyList, removeElements, addElements, shuffleList, reverseList, cycleList, multiplyList]