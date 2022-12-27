% Langford 4
% 8 slots for {11,22,33,44}
%

langford(L,L) :- length(L,4).

langford(L0,Soln) :-
            domain(8,K,N1,N2),
        
            \+ member([K,_,_],L0),
            \+ member([_,N1,_],L0),
            \+ member([_,_,N2],L0),
            \+ member([_,N2,_],L0),
            \+ member([_,_,N1],L0),

            langford([[K,N1,N2]|L0],Soln).


domain(Len,K,N1,N2) :-
        val(K),
        aindex(N1),
        N1max is Len - (K + 1), 
        N1 =< N1max,
        N2 is N1 + (K + 1).


val(1).
val(2).
val(3).
val(4).

aindex(1).
aindex(2).
aindex(3).
aindex(4).
aindex(5).
aindex(6).
