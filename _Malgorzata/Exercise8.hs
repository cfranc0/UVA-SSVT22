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
accuses Arnold name = (name==Matthew || name==Peter) && not(accuses Peter name) && accuses Matthew name
--Implementation of statement  "Carl: What Arnold says is not true"
accuses Carl name= not(accuses Arnold name)


-- Here, we look up a person's given name to see if any of the boys have accused him, and we add the name of the accuser.
accusers ::  Boy -> [Boy]
accusers y = [ x | x <- boys, accuses x y ]

--guilty :: [Boy]
--This function should check each boy's accusers and return the name of the boy with three accusers.

--honest ::  [Boy]
--This function should call accusers function with the value returned from guilty at index 0.  
