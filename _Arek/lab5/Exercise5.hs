module Exercise5 where

-- Time spent: x minutes -- started: 19.21

-- Implement function(s) that calculate the conjectures: properties that are equivent, whose cases are subsets of other properties, etc.

import Data.List
import Test.QuickCheck
import Mutation
import MultiplicationTable

-- Time spent: x minutes -- started: 19.03
--Implement function(s) that calculate the conjectures: properties that are equivent, 
-- whose cases are subsets of other properties, etc.
-- Deliverables: implementations, documentation of approach, indication of time spent.

type PropertyList = [[Integer] -> Integer -> Bool]
type Mutator = [Integer] -> Gen [Integer]

--find out which sets of properties result in similar number of killed mutations








































