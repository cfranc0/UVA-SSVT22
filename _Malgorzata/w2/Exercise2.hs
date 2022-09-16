data Shape = NoTriangle | Equilateral
                | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z
    |  x + y >= z || x + z >= y || z + y >= x = NoTriangle
    | z == y  && y == x = Equilateral
    | x^2 + y^2 == z^2 || x^2 + z^2 == y^2 || z^2 + y^2 == x^2 = Rectangular
    | z == x || z == y || x == y = Isosceles
    | otherwise = Other

    