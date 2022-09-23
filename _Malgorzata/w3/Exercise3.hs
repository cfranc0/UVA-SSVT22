module Exercise3 where
import Lecture3


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