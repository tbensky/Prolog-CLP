langford(L) :-

    L = [A,B,C,D,E,F,G,I],
    val(A), val(B), val(C), val(D), val(E), val(F), val(G), val(I),

    rule(L,1),
    rule(L,2),
    rule(L,3),
    rule(L,4).


rule(L,K) :-    nth1(I,L,K),
                J is I + K + 1,
                nth1(J,L,K), 
                count2(L,K).  
  
count2(L, E) :-
    include(=(E), L, L2), 
    length(L2, 2).

val(1). 
val(2).
val(3).
val(4).