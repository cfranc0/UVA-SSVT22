data Boy = Matthew | Peter | Jack | Arnold | Carl
                  deriving (Eq,Show)
      
boys = [Matthew, Peter, Jack, Arnold, Carl]

--function declaration for when a boy accuses onother one 

accuses ::  Boy -> Boy -> Bool
--Implementation of statement like "Mathew says Carl dont do it and neither did he"
accuses Matthew name = not (name==Carl) && not (name==Matthew) 
--Implementation of statement like "Peter: It was Matthew or it was Jack.
accuses  Peter name =name == Jack || name== Matthew
--Implementation of statement like "Jack: Matthew and Peter are both lying"
accuses Jack name =not (accuses (Jack)name) && not (accuses(Matthew) name)
--Implementation of statement like "Arnold: Matthew or Peter is speaking the truth, but not both."
accuses Arnold name = (name==Jack || name==Peter) && not(accuses (Jack)name)  && (accuses(Matthew) name)
--Implementation of statement like "Carl: What Arnold says is not true"
accuses Carl name= not(accuses(Arnold) name)



--accusers ::  Boy -> [Boy]

--guilty, honest ::  [Boy]
