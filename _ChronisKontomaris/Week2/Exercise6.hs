import Data.List
import Data.Maybe (fromMaybe)
import Data.Char
import Test.QuickCheck


--we create two simple Lower case and Upper case lists
alphabetUp=['A' .. 'Z']
alphabetLo=['a' .. 'z']


--Specification of the rot13 encoding :
-- rot13 uses the normal alphabet and substitutes each one of  the letters in the char list to the equivalent letter of the 13 letters away in the alphabet 
-- if some letter reaches the end of the alphabet after the 13 letter addition, then we go to the start of the alphabet and add the remaining letters from the 13 on when we reached the end 

--here we have a function that takes an integer and returns an array list of every digit of the integer using map
digits :: Integer -> [Int]
digits ch = map (\x -> read [x] :: Int) (show ch)



--https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-List.html
--https://www.anycodings.com/1questions/3452690/haskell-how-to-get-index-of-elem-in-list
--Here we created a functions from resources mentioned above, 
--that searches for an element in a list and returns positive index integer if found else it returns negative integer
elemIndex' :: Eq a => a -> [a] ->  Int
elemIndex' x = fromMaybe(-1).elemIndex x



rot13 ::  [Char] -> [Char]
--case when list is empty
rot13 []=[]
--we test for each letter if it is Lower Case or Upper case first

rot13 (y:ys)  = if (isLower y) then 
    --next we find the index of the letter in the alphabets list and check if the index + 13 is less than 26 
                    if (((elemIndex' y alphabetLo ) + 13)<26) 
                    --index is less than 26, so we dont need to go to the start of the alphabet
                    --we go again to alphabet to search for the substitute letter after we incremented by 13 positions (this is made with the wildcard !!) and add it to the new list
                            then alphabetLo !! ((elemIndex' y alphabetLo ) + 13) : rot13 ys
                    --if the the index of the letter in the alphabets list and check if the index + 13 is greater than 26, 
                    --than in order to go to the start of the list  we subtract from the new index 26 position in order to get aligned from the start of list
                    else  alphabetLo !!  (((elemIndex' y alphabetLo ) + 13)-26) : rot13 ys 
                else
                    --Here is the same procedure as for lower case letter, but now we search in the Uppercase alphabet list
                    if (((elemIndex' y alphabetUp ) + 13)<26) 
                            then alphabetUp !! ((elemIndex' y alphabetUp ) + 13) : rot13 ys
                     else  alphabetUp !!  (((elemIndex' y alphabetUp ) + 13)-26) : rot13 ys 

