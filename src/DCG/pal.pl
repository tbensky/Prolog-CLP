palindrome(A, B) :- A=B.
palindrome([_|A], A).
palindrome([A|B], C) :-
    palindrome(B, D),
    D=[A|C].

word([a,b,c,b,a]).
word([a,b,c,d,e]).

t --> [].
t --> [a].
t --> [b], [c].

diff(X,Y).

app_dl1(A,B,B,A).
app_dl2().

dl([],L-L).
dl([H|T],[H|L1]-L2) :- dl(T,L1-L2).

/*

?- word(L),palindrom(L,[]).
Correct to: "palindrome(L,[])"? yes
L = [a, b, c, b, a] ;


?- palindrome(L,[]).
L = [] ;
L = [_] ;
L = [_A, _A] ;
L = [_A, _, _A] ;
L = [_A, _B, _B, _A] ;
L = [_A, _B, _, _B, _A] ;
L = [_A, _B, _C, _C, _B, _A] ;
L = [_A, _B, _C, _, _C, _B, _A] 

*/

