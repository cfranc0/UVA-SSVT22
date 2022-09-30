import Exercise4
import LTS

--here we have the out funcion which takes an IOLTS set and a state list and gives it to our secondary function out' as an LTS set
out :: IOLTS->[State] ->[Label]
--we call our secondary LTS function with the union of the IOLTS input and ouput labels to match the LTS parameters 
out  (states,labels1,labels2,transitions,state) x=out'  (states,labels1++labels2,transitions,state) x

--here we have our secondary function which takes the LTS set and a state set and tries to find the labels of the states
out' :: LTS->[State] ->[Label]
--case for when the state list is empty, where we return the delta LTS datatype
out'  (states,labels,transitions,state) []=[delta]
--otherwise we just iterate through each on of the states 
out'  (states,labels,transitions,state) (x:xs)=
--if the next item is not the end then we take the label of the specific state and call recursively the out' function for the rest of the states
    if xs /=[] then 
        [z | (i,z,j)<-transitions,i==x ] ++ out' (states,labels,transitions,state) (xs)
    else
--otherwise we just add the label of the state to our return label list
        [z | (i,z,j)<-transitions,i==x ] 

--attempt to make the ioco function, but it does not work 
--ioco :: IOLTS->IOLTS -> Bool

--ioco impl (r,y,z,k,e) =  not (elem False ([ (out (r,y,z,k,e) ((r,y,z,k,e) `after` [x])) == (out impl (impl`after` [x])) | x<-y++z ]) )