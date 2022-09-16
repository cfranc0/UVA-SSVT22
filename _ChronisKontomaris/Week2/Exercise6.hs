import Data.List
import Data.Maybe (fromMaybe)
import Data.Char

alphabetUp=['A' .. 'Z']
alphabetLo=['a' .. 'z']


--Specification of the rot13 encoding :
-- rot13 uses the normal alphabet and substitutes each one of  the letters in the char list to the equivalent letter of the 13 letters away in the alphabet 
-- if some letter reaches the end of the alphabet after the 13 letter addition, then we go to the start of the alphabet and add the remaining letters from the 13 on when we reached the end 

digits :: Integer -> [Int]
digits ch = map (\x -> read [x] :: Int) (show ch)



--https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-List.html
--https://www.anycodings.com/1questions/3452690/haskell-how-to-get-index-of-elem-in-list
elemIndex' :: Eq a => a -> [a] ->  Int
elemIndex' x = fromMaybe(-1).elemIndex x



rot13 ::  [Char] -> [Char]
--case when list is empty
rot13 []=[]
rot13 (y:ys)  = if (isLower y) then 
                    if (((elemIndex' y alphabetLo ) + 13)<26) 
                  
                            then alphabetLo !! ((elemIndex' y alphabetLo ) + 13) : rot13 ys
                    else  alphabetLo !!  (((elemIndex' y alphabetLo ) + 13)-26) : rot13 ys 
                else
                    if (((elemIndex' y alphabetUp ) + 13)<26) 
                            then alphabetUp !! ((elemIndex' y alphabetUp ) + 13) : rot13 ys
                     else  alphabetUp !!  (((elemIndex' y alphabetUp ) + 13)-26) : rot13 ys 

