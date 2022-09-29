module Euler15 where

--Euler 15: Lattice paths
--in grid any grid n x n there are total of n*2! combinations however since we only move right and down, we divide by n! of rights times n! of downs
--time started: 19:44
--time ended: 20:02 
--time taken: 15 mins

--create a function to calculate a factorial value
factorial :: (Num a, Enum a) => a -> a
factorial x = product[1..x]

--total of 40! unique moves divided by 20! rights * 20! downs
euler15 :: Integer
euler15 = factorial 40 `div` (factorial 20 * factorial 20)