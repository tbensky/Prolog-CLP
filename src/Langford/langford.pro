% Turbo Prolog (yes, the 1980s software) implementation
% of Langford pairing for {11,22,33,44}
domains
	llist = l(list); i(integer)
	list = llist*

predicates
	length(list,integer)
	domain(integer,integer,integer,integer)
	langford(list,list)
	member(llist,llist)
	val(integer)
	aindex(integer)
	
clauses

member(X,l([X|_])).
member(X,l([_|T])) :- member(X,l(T)).

length([],0).
length([_|Xs],L) :- length(Xs,N), L = N + 1.

langford(L,L) :- write(L), nl, length(L,4).

langford(L0,Soln) :-
            domain(8,K,N1,N2),
       
            not(member(l([i(K),_,_]),l(L0))),
            not(member(l([_,i(N1),_]),l(L0))),
            not(member(l([_,_,i(N2)]),l(L0))),
            not(member(l([_,i(N2),_]),l(L0))),
            not(member(l([_,_,i(N1)]),l(L0))),
            langford([l([i(K),i(N1),i(N2)])|L0],Soln).


domain(Len,K,N1,N2) :-
        val(K),
        aindex(N1),
        N1max = Len - (K + 1), 
        N1 <= N1max,
        N2 = N1 + (K + 1).


val(1).
val(2).
val(3).
val(4).

aindex(1).
aindex(2).
aindex(3).
aindex(4).
aindex(5).
aindex(6).

goal
	langford([],L), write(L), nl.