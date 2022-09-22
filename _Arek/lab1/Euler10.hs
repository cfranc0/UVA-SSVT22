import Lab1

-- time taken 5min

--find all primes between 2 and 2000000 and sum them
euler10 :: Integer
euler10 = sum(2 : filter prime [3..2000000])