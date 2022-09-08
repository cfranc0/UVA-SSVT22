-- Time spent: 2h

import Data.List
import Math.NumberTheory.Logarithms

-- Source: Lab1.hs
reversal :: Integer -> Integer
reversal = read . reverse . show

-- Source: https://stackoverflow.com/a/3964069/9754741
digits :: Integer -> [Int]
digits = map (read . return) . show

digits' :: Int -> [Int]
digits' = map (read . return) . show

luhn :: Integer -> Integer
luhn n
    | n < 0 = luhn ((-1) * n)
    | otherwise = toInteger $ 10 - (foldl (+) 0 (map (\i ->
        foldl (+) 0 $ digits' (
            ((digits . reversal) n)!!i * (2 - i `mod` 2)
        )) [0..integerLog10 n])
        ) `mod` 10

-- 
-- Source: https://www.forbes.com/advisor/credit-cards/what-does-your-credit-card-number-mean/

-- lastDigit

-- isAmericanExpress :: Integer -> Bool
-- isAmericanExpress n = luhn . init . digits

-- isMaster :: Integer -> Bool


-- isVisa :: Integer -> Bool



