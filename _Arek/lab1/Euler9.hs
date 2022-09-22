-- time taken 1hr

--generate 2 lists upto a a value of 1000 (since we know it cant be larger than that) and find which numbers will satisfy the equation, when found, calculate the product 
euler9 :: Integer
euler9 = product(head [[a, b, c] 
    | a <- [1..1000], b <- [1..1000], 
    let c = 1000 - a - b, a^2 + b^2 == c^2])
