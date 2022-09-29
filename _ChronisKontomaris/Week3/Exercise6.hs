-- Time spent: 180 minutes --


<<<<<<< HEAD
import Lecture3 ( parse, Form(Dsj, Cnj, Neg, Prop) )
=======
import Lecture3
>>>>>>> 9bb8dee3f85932dcce61344367c4f62dcc750ce0


type Clause=[Int]
type Clauses= [Clause]



-- we will turn disjunctions into clauses and conjunction of clauses into separate clauses
--From the exericse details when understand that when we identify a disjuction, we add elements to the same list ( Clause)
--In conjunctive normal form, statements in Boolean logic are conjunctions of clauses with clauses of disjunctions.
cnf2cls :: Form -> Clauses

--tranforming items from disjunctions into items of the clause 
cnf2cls (Dsj x) = map dsjtoInt x
--generate lists of clauses 
cnf2cls (Cnj x) = map dsjtoInt x

--convertion of literals into integers with the corrent integer sign
propToInt :: Form->Int
propToInt (Neg(Prop r))= -r
propToInt (Prop r)=r

--function that takes items from the disjunction items and put them in clause list
dsjtoInt :: Form->[Int]
dsjtoInt (Dsj e)=map propToInt e



--testing on an example with a parsed formula to check if the number of Clauses in our result is the same as the number of  Disjuctions,
--In our case we look for the nubmer of "+" given in the parsed Formula to equal the number of clauses in the result after our conversion   
-- we use head function, because the parse function returns [Form] type, so we just take the first item which is what we want
-- We didnt automate the testing for this exercise, but we used an example  of the parsed formula "*(+(1 2) +(3 4))" which is equivalent to (A OR B) AND (C OR D) to test it 
-- changing the parse formulas can test our implementation further 
formtest=head (parse "*(+(1 2) +(3 4))")
result=cnf2cls  formtest

--we take the number of Disjunctions of the parsed formula
dislength= length ([x | x <- (show formtest) , x=='+'])
--we test the number of Disjunctions with the number of clauses
testconversion = dislength == length(result)