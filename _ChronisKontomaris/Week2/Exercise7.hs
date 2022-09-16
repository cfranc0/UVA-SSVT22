import Data.List
import Data.Maybe (fromMaybe)
import Data.Char (digitToInt)
import Data.Bits



--countries code huge list 

countries_list=["AL","AD","AT","AZ","BH", "BY", "BE", "BA", "BR", "BG", "CR", "HR", "CY", "CZ", "DK", "DO", "SV", "EE", "FO", "FI", "FR", "GE", "DE", "GI", "GR", "GL", "GT", "HU", "IS", "IQ", "IE", "IL", "IT", "JO", "KZ", "XK", "KW", "LV", "LB", "LI", "LT", "LU", "MK", "MT", "MR", "MU", "MD", "MC", "ME", "NL", "NO", "PK", "PS", "PL", "PT", "QA", "RO", "LC", "SM", "ST", "SA", "RS", "SC", "SK", "SI", "ES", "SE", "CH", "TL", "TN", "TR", "UA", "AE", "GB", "VA", "VG"]
countries_lengths=["28", "24", "20", "28", "22", "28", "16", "20", "29", "22", "22", "21", "28", "24", "18", "28", "28", "20", "18", "18", "27", "22", "22", "23", "27", "18", "28", "28", "26", "23", "22", "23", "27", "30", "20", "20", "30", "21", "28", "21", "20", "20", "19", "31", "27", "30", "24", "27", "22", "18", "15", "24", "29", "28", "25", "29", "24", "32", "27", "25", "24", "22", "31", "24", "19", "24", "24", "21", "23", "24", "26", "29", "23", "22", "22", "24"]





--here we create a simple Upper case alphabet to test the iban numbers
alphabet=['A' .. 'Z']

--https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-List.html
--https://www.anycodings.com/1questions/3452690/haskell-how-to-get-index-of-elem-in-list
elemIndex' :: Eq a => a -> [a] ->  Int
elemIndex' x = fromMaybe(-1).elemIndex x


--As in exercise 6, we have a function that takes an integer and returns an array list of every digit of the integer using map

digits :: Integer -> [Int]
digits ch = map (\x -> read [x] :: Int) (show ch)

--in this function we take an interger array and return the concatenated integer in string 
concats :: [Int] -> String
concats []=[]
concats (y:ys)= show y ++  concats  ys


--in this function we replace each letter with their equivalent integer number (for example A=10,B=11 .. etc)
-- this is implemented by finding each letter index  in the alphabet and return as an that it is added the constant 10   
replace ::  [Char] -> [Int]
--case when list is empty
replace []=[]
replace(y:ys)  = if ((elemIndex' y alphabet ) > -1) then ((elemIndex' y alphabet ) + 10) : replace ys else digitToInt y : replace ys



--in this function we rotate one item of the Char list 
--this function will be used later multiple times to rotate the first 4 characters of the iban number
rot :: [Char] -> [Char]
rot []=[]
rot (x:xs) = xs ++ [x]

--import Data.Sequence

-- imported Data.Sequence from the hackage.haskell website https://hackage.haskell.org/package/containers-0.6.6/docs/Data-Sequence.html
-- used isInfixOf in order to find if any of the various countries codes are inside the given iban
iban="FR7630006000011234567890186"
--here test that the prefix "FR" of the hardcoded iban string is indeed in the countries_codes list and hence its valid iban in respect to the countries specification
countries_codes_test :: [[Char]]->Bool
countries_codes_test []=False
countries_codes_test (g:gs)= if (isInfixOf g iban) then True  else countries_codes_test gs



--for each iban we do the following procedure:
-- Step 1: we rotate the first 4 items 
y = take 5 $ iterate rot iban
--Step2: because take 5 on iterate gives us 4 different results, 
--we take only the last where the 4 characters have move to the end

result= last y
--Step3 : we use the function "replace"  that we made ,in order to convert each alphanumeric character,
-- with its equivalent letter in the alphabet (starting with number 10 for A)

replaced_final=replace result
--Step4 : we concatenate the Int array into string
finalconcat=concats replaced_final
--Step 5 : we read the concatenated string as integer 
finalint=read finalconcat :: Integer

--Step6 : we perform mod97==1 expression on the iban to see if its valid 
validation=finalint `mod`97==1
--if validation variable is true than the iban number is valid 

--Unfortunately, we were not able to implement the fully working program for multiple iban numbers,
-- so we used the hardcoded iban string as a test case to showcase our thoughts in this exercise
-- We would have implemented a function that does all of afformentioned steps and check them for each one of the iban numbers  

        
 