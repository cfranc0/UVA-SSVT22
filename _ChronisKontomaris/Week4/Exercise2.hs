import LTS
import Data.Char
import Test.QuickCheck
import Control.Monad
--instance arbitrary where
   --arbitrary = State <$> choose (0,9)
--   =choose(0,9)

--genForm = sized genForm'

alphabet=['a'..'z']


-- fibs(0) = 1
-- fibs(1) = 1
-- fibs(n+2) = fibs(n) + fibs(n+1)
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
--genForm' :: Gen [Int]
--genForm' 0 = arbitrary
--genForm' n | n>0 = arbitrary : (genForm' $ n-1)
    
