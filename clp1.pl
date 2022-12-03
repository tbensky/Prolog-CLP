:- use_module(library(clpfd)).


go(X,Y,Z) :- 
			X #= 4, 
			Y #= X + Z,  
			Z #> 2.