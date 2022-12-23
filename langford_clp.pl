:- use_module(library(lists)).
:- use_module(library(clpfd)).

langford(_,Ans) :- 
					Ans = [A,B,C,D,E,F], 
					Ans ins 1..3, 
					check(Ans,1).

check([],_).
check([1,_,1|T],1) :- F = [1,2,3,4], maplist(nth1(check(T,1).  
	



go(Ans) :- langford([1,1,2,2,3,3],Ans).