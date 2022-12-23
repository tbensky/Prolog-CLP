:- use_module(library(clpfd)).
:- use_module(library(apply)).


as --> [].
as --> [_], as.

... --> [] | [_], ... .

/*
generate a list sequence required of a given number
Ex: N=1, Ls=[1,_,1]
    N2=, Ls=[2,_,_,2]
    etc
*/

gen_list(N,Ls) :- Need #= N+2, length(Ls,Need), phrase(as,Ls), nth1(1,Ls,N), last(Ls,N).

/*
check if Elem appears twice in list L.
*/

mkz(X,X) :- \+ var(X).
mkz(X,0) :- var(X).

check_two(L,Elem) :- include(#=(Elem),L,L2), length(L2,2).

check_pairs(_,[]).
check_pairs(Q,[H|T]) :- check_two(Q,H), check_pairs(Q,T). 


go(Ls) :- 
            Ls = [A,B,C,D,E,F,G,I],
            Ls ins 1..4,
            check_two(Ls,1).
          
            
            %gen_list(N,L),
            %phrase((...,L,...),Ls),
            
            %write(Ls), nl,
            %check_two(Ls,N).
            
          
