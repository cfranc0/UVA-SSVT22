

module Exercise4
where 
import LTS

--implementation of the after infix funtion
after :: IOLTS ->[Label]-> [State]
-- here we take an IOLTS transition set and we pass it as an LTS set with its union of labels and call our LTS function which computes the after infix 
after (states,labels1,labels2,transitions,state) x =after'  (states,labels1++labels2,transitions,state) x

--here we implemented the function after' which takes ant LTS transition set and computes the state from the 'after' infix
after' :: LTS -> [Label] -> [State]
--we iterate through each the labels given for the after computation  
after'  (states,labels,transitions,state) (x:xs)= 
--next we check in every iteration whether the length of the remaining label list is more than one element
    if (length(xs)>0) then
--if we have more than one element we check whether the next state of the last element reaches the end of the tree
        if concat[[ i | (i,l,g) <- transitions, z ==i] | (r,y,z)<-transitions,y==(last xs)] /= [] then
--if it does not reach the end, then we just add the states 
             [ z| (r,y,z)<-transitions,y==x]  ++ after' (states,labels,transitions,state) (xs)
        else 
--if it reaches the end , then we will return the empty state
            concat[[ i | (i,l,g) <- transitions, z ==i] | (r,y,z)<-transitions,y==(last xs)]
            
    else
--Otherwise we just add the state to our state list
        [ z| (r,y,z)<-transitions,y==x && r==state]




