
lastDigit :: Integer -> Integer
lastDigit x 
        | x > 99 = x `rem` 10
        | otherwise = x

multiply :: Integer -> Integer
multiply x 
    | k > 9 = (k `mod` 10) + 1
    | otherwise = k
    where k = lastDigit x

luhn :: Integer -> Integer
luhn x 
    | x < 10 = x
    | otherwise = sum ((multiply x) + (luhn x `div` 10))


-- isAmericanExpress :: Integer -> Bool


-- isMaster :: Integer -> Bool


-- isVisa :: Integer -> Bool



