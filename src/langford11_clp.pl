:- use_module(library(clpfd)).

langford(L) :-

    L = [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_],
    L ins 1..11,

    rule(L,1),
    rule(L,2),
    rule(L,3),
    rule(L,4),
    rule(L,5),
    rule(L,6),
    rule(L,7),
    rule(L,8),
    rule(L,9),
    rule(L,10),
    rule(L,11),

    label(L),
    
    count2(L,1),
    count2(L,2),
    count2(L,3),
    count2(L,4),
    count2(L,5),
    count2(L,6),
    count2(L,7),
    count2(L,8),
    count2(L,9),
    count2(L,10),
    count2(L,11).

rule(L,K) :-    element(I,L,K),
                J #= I + K + 1,
                element(J,L,K).

 
 count2(L, E) :-
    include(=(E), L, L2), 
    length(L2, 2).
  