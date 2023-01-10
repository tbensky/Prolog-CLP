:- include('words.pl').

print_list([]) :- nl.
print_list([H|T]) :- write(H), nl, print_list(T).


go(L) :-
        findall(X,word([A,B,_,B,A],_,X), L),
        print_list(L).
