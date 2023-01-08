:- use_module(library(clpfd)).

langford(L) :-

    L = [
            _,_,_,_,_,_,_,_,_,_,
            _,_,_,_,_,_,_,_,_,_,
            _,_,_,_,_,_,_,_,_,_
        ],
    L ins 1..15,

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
    rule(L,12),
    rule(L,13),
    rule(L,14),
    rule(L,15).

rule(L,K) :-    nth1(I,L,K),
                J #= I + K + 1,
                nth1(J,L,K).