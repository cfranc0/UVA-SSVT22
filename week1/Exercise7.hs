-- Time spent: 6h

import Data.List
import Math.NumberTheory.Logarithms
import Test.QuickCheck

-- Source: https://stackoverflow.com/a/3964069/9754741
digits :: Integer -> [Integer]
digits n = [read[x] :: Integer | x <- (show n)]

calculateLuhn :: Integer -> Integer
calculateLuhn n
    | n == 0    = 0
    | n < 0     = calculateLuhn ((-1) * n)
    | otherwise = (10 - (foldl (+) 0 (map (\i ->
        foldl (+) 0 $ digits (
            (reverse $ digits n)!!i * (2 - (toInteger i) `mod` 2)
        )) [0..integerLog10 n])
        ) `mod` 10) `mod` 10

lastDigit :: Integer -> Integer
lastDigit n = toInteger $ (digits n)!!(integerLog10 n)

--
luhn :: Integer -> Bool
luhn n = calculateLuhn(n `div` 10) == lastDigit n

{-
TESTING:
Using quickcheck to make sure the luhn number calculated by the function is
always less than 10, since the checksum number need to be a single digit
-}
prop_lessThan10 :: Integer -> Bool
prop_lessThan10 n = calculateLuhn n < 10

-- 
-- Source: https://www.forbes.com/advisor/credit-cards/what-does-your-credit-card-number-mean/

{-
The following two functions were implemented to check the validity of a Visa
card number as the source cited above does say that Visa uses digit 13 as the
checksum digit. This is, however, not confirmed by any other source.
Specifically, this is "Visa Solution" which says that tipically the check digit
is at the end
https://developer.cybersource.com/docs/cybs/en-us/test-data/developer/all/so/test-data/best_practices_intro/checking_digit_scheme.html


splitIntAt :: Integer -> Int -> (Integer, Integer)
splitIntAt n p = ((n `div` 10^p), (n `mod` 10^p))

luhn' :: Integer -> Int -> Bool
luhn' n p = let (h, t) = splitIntAt n p in
    luhn (h) == (digits n)!!(p-1)
-}


isAmericanExpress :: Integer -> Bool
isAmericanExpress n = integerLog10 n == 14 && ((n `div` 10^(integerLog10 n - 1)) == 34 || (n `div` 10^(integerLog10 n - 1)) == 37) && luhn n

isMaster :: Integer -> Bool
isMaster n = integerLog10 n == 15 && (n `div` 10^(integerLog10 n)) == 5 && luhn n

isVisa :: Integer -> Bool
isVisa n = integerLog10 n >= 12 && integerLog10 n <= 15 && (n `div` 10^(integerLog10 n)) == 4 && luhn n

{-
TESTING:
To check the correctness of the implementation the functions need to be tested
against some known good credit card numbers generated online (they are not our
credits cards, don't bother trying them on Amazon ;) )
- The first test wants to make sure the luhn function correctly checks the
  valid credit card numbers
- The second test checks that the luhn function correctly catches modified
  credit card numbers.
  Each credit card number gets modified by adding +1 in random positions
- Each credit card circuit checker gets checked by:
  * Making sure the checker does correctly match only the correct credit card
    type
  * Making sure the checker correctly recognizes tampered credit card numbers

The known good cards are going to them be changed in order to make them invalid
and check if the functions are able to catch that.
All the cards numbers are then checked again 
-}

test_visa   = [4875115814908042,4662791634399708,4875115251280749,4875119511287809,4875119046629319,4662797156532592,4662792968036718,4875112109552791,4875113630841729,4662795270313428,4875117059299921,4875116793110683,4662794467973896,4662794264521336,4875119736047624,4875111811102671,4662798746707017,4875114339426994,4875111935924315,4662794808686959]
test_master = [5325487340758941,5288735713909037,5325483320588879,5325486992941201,5325487499884779,5288737028324217,5325489159873860,5325488901505457,5288735949358934,5325487684321157,5288732312534571,5325482955691966,5325489252610151,5325481915920085,5325481269825633,5325488544516887,5288736626058268,5288737789872065,5325485823066402,5325486210189534]
test_amex   = [340000278231591,341374965258830,370353486641850,370686523870002,370686298386945,340000504888438,370686533190227,370353174924790,341374578247030,341374810023967,370353818924800,370353318444697,370686784885715,340000855001359,341374660128841,341374535678889,370686957320664,340000340511244,341374697591052,370686117732782]

prop_testCorrectLuhn :: Bool
prop_testCorrectLuhn = all (== True) (map (\n -> luhn n) test_visa) && all (== True) (map (\n -> luhn n) test_master) && all (== True) (map (\n -> luhn n) test_amex)

prop_testTamperedLuhn :: Positive Integer -> Bool
prop_testTamperedLuhn (Positive offset) = all (== False) (map (\n -> luhn (n + 10^(offset `mod` 16))) test_visa) && all (== False) (map (\n -> luhn (n + 10^(offset `mod` 16))) test_master) && all (== False) (map (\n -> luhn (n + 10^(offset `mod` 15))) test_amex)


prop_testCorrectVisa :: Bool
prop_testCorrectVisa = all (== True) (map (\n -> isVisa n) test_visa) && all (== False) (map (\n -> isVisa n) test_master) && all (== False) (map (\n -> isVisa n) test_amex)

prop_testTamperedVisa :: Positive Integer -> Bool
prop_testTamperedVisa (Positive offset) = all (== False) (map (\n -> isVisa (n + 10^(offset `mod` 16))) test_visa) && all (== False) (map (\n -> isVisa (n + 10^(offset `mod` 16))) test_master) && all (== False) (map (\n -> isVisa (n + 10^(offset `mod` 15))) test_amex)


prop_testCorrectMaster :: Bool
prop_testCorrectMaster = all (== True) (map (\n -> isMaster n) test_visa) && all (== False) (map (\n -> isMaster n) test_master) && all (== False) (map (\n -> isMaster n) test_amex)

prop_testTamperedMaster :: Positive Integer -> Bool
prop_testTamperedMaster (Positive offset) = all (== False) (map (\n -> isMaster (n + 10^(offset `mod` 16))) test_visa) && all (== False) (map (\n -> isMaster (n + 10^(offset `mod` 16))) test_master) && all (== False) (map (\n -> isMaster (n + 10^(offset `mod` 15))) test_amex)


prop_testCorrectAMEX :: Bool
prop_testCorrectAMEX = all (== True) (map (\n -> isAmericanExpress n) test_visa) && all (== False) (map (\n -> isAmericanExpress n) test_master) && all (== False) (map (\n -> isAmericanExpress n) test_amex)

prop_testTamperedAMEX :: Positive Integer -> Bool
prop_testTamperedAMEX (Positive offset) = all (== False) (map (\n -> isAmericanExpress (n + 10^(offset `mod` 16))) test_visa) && all (== False) (map (\n -> isAmericanExpress (n + 10^(offset `mod` 16))) test_master) && all (== False) (map (\n -> isAmericanExpress (n + 10^(offset `mod` 15))) test_amex)
