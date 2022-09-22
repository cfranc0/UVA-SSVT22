import Data.List
import Test.QuickCheck    
import Lab1

-- Time spent: xhr (started 18:17)

consecutive101Prime x = sum (take 101 (2 : filter prime [3+x..]))

c101 x = prime (consecutive101Prime x)