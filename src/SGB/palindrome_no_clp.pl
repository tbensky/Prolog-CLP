:- include('words.pl').

go :-
        word([A,B,C,D,E]),
        A = E,
        B = D,
        write([A,B,C,D,E]).