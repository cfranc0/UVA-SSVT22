module Exercise4 where
import Lecture3
import Test.QuickCheck
import Control.Monad
import SetOrd

-- Time spent: 60 minutes

instance Arbitrary Form where
    arbitrary = Prop <$> choose (1,9)

genForm = sized genForm'

genForm' :: Int -> Gen Form
genForm' 0 = arbitrary
genForm' n | n>0 =
            oneof  [arbitrary,
            liftM2 twoArgCnj subGenForm subGenForm,
            liftM2 twoArgDsj subGenForm subGenForm,
            liftM2 Equiv subGenForm subGenForm,
            liftM2 Impl subGenForm subGenForm]
    where subGenForm = genForm' (n `div` 2)

twoArgCnj :: Form -> Form -> Form
twoArgCnj x y = Cnj [x, y]

twoArgDsj :: Form -> Form -> Form
twoArgDsj x y = Dsj [x, y]
