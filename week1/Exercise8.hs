

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
accuses Arnold name = (name == Matthew || name == Peter) && not(accuses Peter name)  && accuses Matthew name
--Implementation of statement  "Carl: What Arnold says is not true"
accuses Carl name = not(accuses Arnold name)



--Go on all boys to find how many accusers has each one of them
accusers ::  Boy -> [Boy]
accusers acc = [ name | name <- boys, accuses name acc ]

--From the given exercise, if we assume that the teacher is right, we have that three of the boys always tell the truth . 
--So if we find that three of the boys accuses the same persons as thiefs, than that is true.
-- If some boy has less then three accusers it means that at least one truth telling boy excluded him.
-- and if some boy has four or more accusers it means there is lier between them.
guilty ::  [Boy]
guilty = [name | name <- boys, length (accusers name)==3]

--This function calls accusers function with the value returned from guilty at index 0 since there is only one theft.
honest ::  [Boy]
honest = accusers $ head guilty

--Time spent 3h