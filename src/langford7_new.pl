langford(L,L) :- length(L,7).

langford(L0,Soln) :-
            domain(14,K,N1,N2),
        
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
val(5).
val(6).
val(7).

aindex(1).
aindex(2).
aindex(3).
aindex(4).
aindex(5).
aindex(6).
aindex(7).
aindex(8).
aindex(9).
aindex(10).
aindex(11).
aindex(12).