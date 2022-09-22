import Data.List

{-
ROT13- rotate by 13 places. It replaces a letter with the 13th
letter after it in the alphabet.
ROT13(ROT13(x)) = x for any basic Latin-alphabet text x

-}
f :: Char -> Char
f c 
    | index > 12 = ['a','b','c','d','e','f','g','h','i','j','k','m', 'n', 'o', 'p','r','s','t', 'u', 'w', 'y']  !! (index + 13) `mod` 25
    | otherwise = ['a','b','c','d','e','f','g','h','i','j','k','m', 'n', 'o', 'p','r','s','t', 'u', 'w', 'y']  !! index
    where index = elemIndex c ['a','b','c','d','e','f','g','h','i','j','k','m', 'n', 'o', 'p','r','s','t', 'u', 'w', 'y'] 


rot13 :: [Char] -> [Char]
rot13 xs = map f xs

--sprawdza index char w alfabecie i jezeli jest > to mod a jak nie to
