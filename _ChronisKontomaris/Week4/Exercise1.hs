-- Time spent: 60 minutes

module Exercise1
where 

import LTS
import Test.QuickCheck


validateLTS :: LTS -> Bool
--checking whether Q is a non empty set of states 
validateLTS ([],_, _, _)=False
--checking whether Q and L are countable sets. This is checked using the maxbound integer to compare it with their length
validateLTS (states, labels, transitionSet, initState) = (length states) < (maxBound :: Int) && (length labels) < (maxBound :: Int) && elem initState states

--checking for IOLTS is done with our helper validateLTS function in which we give the union of input and output labels of the IOLTSs
validateIOLTS :: IOLTS->Bool
-- last check is to ensure that the inputs and ouput labels of the IOLTS set is a disjoint set, and we check that none of the items in input labels belongs to the output labels 
validateIOLTS (states, labels1,labels2, transitionSet, initState)=validateLTS(states,labels1++labels2,transitionSet,initState) && (filter(\x -> elem x labels2) labels1==[])
