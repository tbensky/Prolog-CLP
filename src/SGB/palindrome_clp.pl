:- use_module(library(clpfd)).
:- include('words.pl').

go(Word) :-
        word(_,W,Word),
        write(W), nl,
        W ins 97..122,
        maplist(rule(W),[1,2,3,4,5]),
        write(Word), nl.

rule(W,I) :- 
        element(I,W,A),
        J #= 6 - I,
        element(J,W,A).