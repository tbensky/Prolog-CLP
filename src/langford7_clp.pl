:- use_module(library(clpfd)).

langford(L) :-

    L = [_,_,_,_,_,_,_,_,_,_,_,_,_,_],
    L ins 1..7,

    rule(L,1),
    rule(L,2),
    rule(L,3),
    rule(L,4),
    rule(L,5),
    rule(L,6),
    rule(L,7).

rule(L,K) :-    nth1(I,L,K),
                J #= I + K + 1,
                nth1(J,L,K).