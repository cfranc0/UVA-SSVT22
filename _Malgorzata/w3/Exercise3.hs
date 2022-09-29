module Exercise3 where
import Lecture3

-- Time spent: 60 minutes

distribute :: Form -> Form
distribute (Dsj [f1, Cnj [f2,f3]]) = Cnj [left, right]
  where left = distribute (Dsj [f2, f1])
        right = distribute (Dsj [f3, f1])

distribute (Dsj [Cnj [f2,f3], f1]) = Cnj [left, right]
  where left = distribute (Dsj [f2, f1])
        right = distribute (Dsj [f3, f1])
distribute f = f

cnf :: Form -> Form
cnf f = distribute (nnf (arrowfree f))

{-
Firstly, if there is implification or equality symbol in formula
we apply arrowfree function that exchanges it to dsj or cnj.
Then we apply nnf function on the result that converts negation of dsj or cnj
to negations of props only, repeatedly using De'Morgans laws, example:
~(B v C) to (~B) ^ (~C)
Finally, distribute function changes nested cnj within dsj, example:
A ^ (B v (D ^ E)) to (A) ^ (B v D) ^ (B v E)
source:
https://en.wikipedia.org/wiki/Conjunctive_normal_form
-}