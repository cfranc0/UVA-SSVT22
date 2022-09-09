data Boy = Matthew | Peter | Jack | Arnold | Carl
                  deriving (Eq,Show)
      
    boys = [Matthew, Peter, Jack, Arnold, Carl]

--function declaration for when a boy accuses onother one 
accuses ::  Boy -> Boy -> Bool

--We will try to implement statements like "Mathew says Carl dont do it and neither did he", using Singleton properties

accusers ::  Boy -> [Boy]
Mathew
guilty, honest ::  [Boy]