-- Time spent: 20m

data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c = if (a == b && b == c) then Equilateral
    else if ((a + b < c) || (a + c < b) || (b + c < a)) then NoTriangle
    else if ((a^2 + b^2 == c^2) || (a^2 + c^2 == b^2) || (b^2 + c^2 == a^2)) then Rectangular
    else if ((a == b) || (b == c)) then Isosceles
    else Other

{-
    The correctness of the program was verified mathematically by checking the
    comparisons that the function does.
    - First of all, if all edges are of the same lenght, the triangle is
        equilateral
    - Second we need to make sure the inequality theorem holds for every other
        triange, since the function can be fed valid-looking isosceles values 
        that are actually impossibile for a triange (e.g. 10 1 1)
    - Once the triange is for sure valid, we can use Pythagorean theorem to
        check whether the triangle is rectangular
    - If the sides a pair of sizes is the same, it is defenitely Iscosceles
    - Finally, if none of the previous condisions are met, the triangle is still
        valid but it's not into any of the previous category
-}