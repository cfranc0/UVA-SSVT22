-- Time spent: 2h

module Exercise1 where
import Test.QuickCheck

squareSum :: Integer -> Integer
squareSum n = sum [x^2 | x <- [1..n]]

squareSum' :: Integer -> Integer
squareSum' n = ((n*(n+1)*(2*n+1)) `div` 6)

testSquareSum :: Positive Integer -> Bool
testSquareSum (Positive n) = squareSum n == squareSum' n

-- -- --

cubeSum :: Integer -> Integer
cubeSum n = sum [x^3 | x <- [1..n]]

cubeSum' :: Integer -> Integer
cubeSum' n = (((n*(n+1)) `div` 2)^2)

testCubeSum :: Positive Integer -> Bool
testCubeSum (Positive n) = cubeSum n == cubeSum' n