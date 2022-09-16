import Data.List
--time spent ~2.5 Hours
--luhn ::  Integer -> Bool

--isAmericanExpress, isMaster, isVisa ::  Integer -> Bool

-- For Master card we need to check for the card number to be 16 characters long and to contain numbers 51-55
-- For visa card we would use head function to check on the first digit that it is indeed a 4 
--  //   //   // we would also check the length of the nuber is 13 or 16 digits as well as the luhn algorithm for all the digits 
--For american express we should check the carrds to be 15 digits and start with number 34 or 37 

--Here is the implementation of a simple solution to get the digits in list from the given card number using the map and show functions
-- Intuition for the digits extraction is from a stackoverflow  question : https://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell
digits :: Integer -> [Int]
digits ch = map (\x -> read [x] :: Int) (show ch)




--We created luhn function to take the finalised list of the digits and sum all of them to check if its modulo 10 ( if it is we return true otherwise false)
luhn :: [Int]-> Bool
luhn u= (sum[ x `div`10 + x `mod`10 | x <- xx ]) `mod` 10==0


--We give number 9664711646 as a verified number from the luhn' algorithm valid number generator website https://www.dcode.fr/luhn-algorithm in order to test our solution
x= digits 9664711646
h=length x
-- We then use the length of digits list to create onother list with the sequence 2 and 1's in order to be multiplied 
y=take h (cycle [2,1])
--we apply multiplication of each element of the lists
xx=zipWith(*) x $ y
--We check whether some of the items in the new list after the multiplication are between 10-19 and replaces them with the summation of their digits 
--This is used from the theory of the Luhn Algorithm
x_x=[ x `div`10 + x `mod`10 | x <- xx ]
--Then we Pass the finalised list to the luhn function
testing=luhn x_x






