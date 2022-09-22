
import Lecture3
--Exercise1 
-- since we have the satisfiable function that checks if a Form is satiasfiable,
-- by checking if for any of  all the examples of valuation can give true
-- So for the tautology we just go through all the examples of valuation and check for that it is true for every example 
tautology :: Form->Bool
tautology f =all(\v -> evl v f )(allVals f )

--contradiction is the oposite of tautology so we just negate the evaluation for each item 
contradiction :: Form->Bool
contradiction f=all(\v -> not (evl v f) )(allVals f )


--We use implication as the truth table has the same output when   
entails :: Form->Form-> Bool
entails fex1 fex2 = all(\v -> evl v (Impl fex1 fex2))(allVals (Impl fex1 fex2))


--In order for two forms to be equivalent there must be entailment for both FormA , FormB as well as FormB and FormA
equiv :: Form->Form->Bool 
equiv fex1 fex2 = (entails fex1 fex2) && (entails fex2 fex1)