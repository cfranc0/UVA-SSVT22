import LTS

--type Trace=[Label]

--straces :: IOLTS -> [Trace]
--straces (states, labels1,labels2, transitionSet, initState)=[[r | (e,r,g)<-transitionSet,e==x] | (x,y,z)<-transitionSet]
--(states,labels,transitions,state)=tretmanR
--straces=[[r | (e,r,g)<-transitions, e==x]| (x,y,z)<-transitions]
--straces=[ y| (x,y,z)<-transitions]

straces :: LTS -> [Trace]
--straces (states, labels1, (transition), initState)=[[y++"-"++r| (e,r,g)<-transition, e==z] | (x,y,z)<-transition]
--straces (states, labels1, ((h,j,k):ps), initState)=[[j]]++[ [y] | (x,y,z)<-ps] 
straces (_, _, [], _)=[]
--straces (states, labels1, (h,j,k):ps, initState)=([j]++[ y | (x,y,z)<-ps,k==x] )  : straces (states,labels1,ps,initState) 
straces (states, labels1, (0,j,k):ps, initState)=if(k `mod` 2 /=0 ) then ([j] ++ [s | (v,s,b)<-ps, v `mod` 2 /=0]) : straces (states,labels1,ps,initState) else  ([j] ++ [s | (v,s,b)<-ps, v `mod` 2 ==0]) : straces (states,labels1,ps,initState) 
--straces (states, labels1, (h ,j ,k):ps, initState)=(if (h `mod` 2 /=0) && (k `mod`2 /=0) then [j] else [] ) : straces (states,labels1,ps,initState) 
straces (states, labels1, (h ,j ,k):ps, initState)=[]
--straces (states, labels1, (h,j ,k):ps, initState)=([ y | (x,y,z)<-ps, z`mod`2==0 ] )  : straces (states,labels1,ps,initState) 

(states,labels,transitions,state)=tretmanR
ps2=transitions!!1
(q,w,z)=ps2