:- include('words.pl').

palindrome --> [].
palindrome --> [_].
palindrome --> [X], palindrome, [X].


pal(A, A).
pal([_|A], A).
pal([A|B], C) :-
    pal(B, [A|C]).

go :- word(X,_,_), palindrome(X,[]).