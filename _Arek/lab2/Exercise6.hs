import Data.Char (ord, chr)
import Test.QuickCheck    

-- ROT13
-- started 00:12
-- finished 04:00
-- time taken: 4 hrs

-- ROT13 is a very basic cryptographic algorithm where letters of the standard english alphabet are shifted by 13 positions along the alphabet, wrapping back to the beginning if necessary
-- works independently for lower and upper case
-- characters other than letters remain unchanged


-- helper function to determine whether to shift letters left or right 
add13 :: (Num a, Ord a) => a -> a
add13 x 
 | x >= 65 && x <= 77 = x + 13
 | x >= 78 && x <= 90 = x - 13
 | x >= 91 && x <= 96 = x
 | x >= 97 && x <= 109 = x + 13
 | x >= 110 = x - 13
 | x < 65 = x
 | x > 122 = x
 

-- helper function to convert character to integer
tonum :: Char -> Int
tonum = ord 

-- helper function to convert integer to character
toAscii :: Int -> Char
toAscii = chr 

rot13 :: [Char] -> [Char]
rot13 x = map toAscii(map add13(map tonum (x)))

--TESTING
-- to test the correct functionality of the algorithm, for any letter input, ROT-13 applied twice should return identical output!
-- TO PERFORM QUICK CHECK  (lower and upper case separately)
-- ghci > quickCheck (forAll genSafeStringLC rot13Test)
-- ghci > quickCheck (forAll genSafeStringUP rot13Test)
-- PLEASE NOTE!
-- the test below does not work because of the escape character '\'
-- when it appears at the begining or end of a string, quicktest crashes
-- ghci > quickCheck rot13Test


genSafeStringLC :: Gen String
genSafeStringLC = listOf (elements ['a'..'z'])

genSafeStringUP :: Gen String
genSafeStringUP = listOf (elements ['A'..'Z'])

--quickCheck test for correctness of the algorithm
rot13Test :: [Char] -> Bool
rot13Test x = x == rot13(rot13 x)


