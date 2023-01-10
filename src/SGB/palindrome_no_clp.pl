:- include('words.pl').

go :-
        word([A,B,C,D,E],_,Word),
        A = E,
        B = D,
        write(Word).