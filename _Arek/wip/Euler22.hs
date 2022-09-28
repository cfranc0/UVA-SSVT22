module Euler22 where
import Names
import Data.List 
import Data.Char (ord, chr)

--Euler 22: Names scores
{-
Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.
For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.
What is the total of all the name scores in the file?
-}
--time started: 22:24
--time ended: 23:02
--time taken: 45 min

letterScore :: Char -> Int
letterScore x = ord x - 64

nameScore :: [Char] -> Int
nameScore x = sum(map letterScore x)

sortNames :: [[Char]]
sortNames = sort Names.names

namesScored :: [(Int, Int)]
namesScored = zip [1..length sortNames] $ map nameScore sortNames

euler22 :: Int
euler22 = sum (map (uncurry (*) ) namesScored)

