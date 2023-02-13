%palindrome(A, B) := A = B, write('clause 1'), nl.
palindrome([_|Tail], A) :- Tail = A, write(['clause 2',Tail,A]), nl.
%palindrome([Head|Tail], C) :- write(['clause 3',Tail,[Head|C]]), nl, palindrome(Tail, [Head|C]).

/*

Called with p([a,b,a],[]).

Clause 1: p([a,b,a],[]). FAILS since [a,b,a] != []

Clause 2: Tail=[b,a], A=[], fails since tail [b,a] != [].

Clause 3: p([a,b,a],[]) :- Head=a, Tail=[b,a], C=[], so call is p([b,a], [a|[]]) or just p([b,a],[a])
            Clause 1: p([b,a],[a]) fails since [b,a] != [a].
            Clause 2: Tail = [a], A=[a], succeeds since Tail = A.
            Clause 3: p([b|a],[a]) :- Head=b, Tail=a, C=[a], so call is p([b],[b|[a]) or just p([b],[b|a])

                Clause 1: p([b],[b|a]) fails since [b]!= [b|a]
                Clause 2: A=[b|a] and [_|A] = [_,b|a]fails since [b|a] != [_,b|a].
                Clause 3: p([b],[b|a]) :- Head=b, Tail=[], C=[b|a], so call is p([],[b|[b|a]]) or just p([],[b,b,|a])

                    Clause 1: p([],[b,b|a]) fails since [b] != [b,b|a]
                    Clause 2: p([],[b,b|a]) fails since [] != [b,b|a]
                    Clause 3: p([],[b,b|a]) fails since there is no head or tail to [].

*/

l([A|B]) :- write(A), nl, write(B), nl.


word([a,b,c,b,a]).
word([a,b,c,d,e]).

tt --> [].
tt --> [a].

p --> [].
p --> [_].
p --> [X], p, [X].

diff(X,Y).

app_dl1(A,B,B,A).
app_dl2().

app_dcg --> front, tail.
front(X) --> [X].
tail(Y) --> [Y].

dl([],L-L).
dl([H|T],[H|L1]-L2) :- dl(T,L1-L2).

%app(L1,L2,L2,L4,L1,L4).
app(A-B,B-C,A-C).
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

