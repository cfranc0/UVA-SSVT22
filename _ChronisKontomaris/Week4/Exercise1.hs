
module Exercise1
where 

import LTS
import Test.QuickCheck


validateLTS :: LTS -> Bool
validateLTS ([],_, _, _)=False
validateLTS (states, labels, transitionSet, initState) = (length states) < (maxBound :: Int) && (length labels) < (maxBound :: Int) && elem initState states

validateIOLTS :: IOLTS->Bool
validateIOLTS (states, labels1,labels2, transitionSet, initState)=validateLTS(states,labels1++labels2,transitionSet,initState) && (filter(\x -> elem x labels2) labels1==[])