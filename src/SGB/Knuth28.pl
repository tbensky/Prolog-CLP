:- include('words.pl').

go :-
        word([A,B,C,D,E]),
        word([F,G,H,I,J]),
        char_code(A,AA),
        char_code(B,BB),
        char_code(C,CC),
        char_code(D,DD),
        char_code(E,EE),
        char_code(F,FF),
        char_code(G,GG),
        char_code(H,HH),
        char_code(I,II),
        char_code(J,JJ),

        Delta1 is abs(AA-FF),
        Delta2 is abs(BB-GG),
        Delta3 is abs(CC-HH),
        Delta4 is abs(DD-II),
        Delta5 is abs(EE-JJ),

        Delta1 = 1,
        Delta2 = 1,
        Delta3 = 1,
        Delta4 = 1,
        Delta5 = 1,
        
        format("~w~w~w~w~w and ~w~w~w~w~w\n",[A,B,C,D,E,F,G,H,I,J]).