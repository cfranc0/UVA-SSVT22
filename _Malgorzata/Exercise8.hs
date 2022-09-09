data Boy = Matthew | Peter | Jack | Arnold | Carl
                  deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

--function declaration for when a boy accuses onother one 

accuses ::  Boy -> Boy -> Bool
--Implementation of statement  "Mathew says Carl dont do it and neither did he"
accuses Matthew name = name /= Carl && name /= Matthew
--Implementation of statement  "Peter: It was Matthew or it was Jack.
accuses  Peter name = name == Jack || name == Matthew
--Implementation of statement  "Jack: Matthew and Peter are both lying"
accuses Jack name = not (accuses Matthew name) && not (accuses Peter name)
--Implementation of statement  "Arnold: Matthew or Peter is speaking the truth, but not both."
accuses Arnold name = (name==Jack || name==Peter) && not(accuses Jack name)  && accuses Matthew name
--Implementation of statement  "Carl: What Arnold says is not true"
accuses Carl name= not(accuses Arnold name)



accusers ::  Boy -> [Boy]
accusers y = [ x | x <- boys, accuses x y ]

--guilty, honest ::  [Boy]