import Data.List

--luhn ::  Integer -> Bool

--isAmericanExpress, isMaster, isVisa ::  Integer -> Bool

-- For Master card we need to check for the card number to be 16 characters long and to contain numbers 51-55
-- For visa cards we need to check that a card  it is 13 or 16 digits and starts with number 4
--For american express we should check the carrds to be 15 digits and start with number 34 or 37 

--Here we implemented a simple solution to get the digits in list from the given card number using the map and show functions
breakch :: Integer -> [Int]
breakch ch = map (\x -> read [x] :: Int) (show ch)



--We created luhn function to take the finalised list of the digits and sum all of them to check if its modulo 10 ( if it is we return true otherwise false)
luhn :: [Int]-> Bool
luhn u= (sum[ x `div`10 + x `mod`10 | x <- xx ]) `mod` 10==0


--We give number 9664711646 as a verified number from the luhn' algorithm valid number generator website in order to test our solution
x= breakch 9664711646
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






