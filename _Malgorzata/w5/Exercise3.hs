module Exercise3 where
import Exercise2
import Test.QuickCheck
import Test.QuickCheck.Gen (Gen(MkGen))
import Control.Monad.Error (liftM)


powerSet :: [a] -> [[a]]
powerSet [] = [[]]
powerSet (x:xs) = [x:y | y<-powerSet xs] ++ powerSet xs


--minimalSet :: (Integer -> [Integer]) -> [[Integer] -> Integer -> Bool] ->
minimalSet fut props n mutator = [x | x <- powerSet props, (countSurvivors n x fut mutator) == fmap (+0) 0]

--minimalSet fut props n mutator = [x | x <- powerSet props,  fmap (*n) (toInteger (countSurvivors n x fut mutator)) == (toInteger (length mutator)) ] 

-- maybe_divide n x fut mutator y = do
--    a <- countSurvivors n x fut mutator
--    b <- y
--    if b == 0 
--      then fail "Division by zero" 
--      else return (a/b)