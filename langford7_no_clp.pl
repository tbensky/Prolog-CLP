langford(Final) :-

  Final = [A,B,C,D,E,F,G,H,I,J,K,L,M,N],
  
  val(A), val(B), val(C), val(D), val(E), val(F), val(G), 
  val(H), val(I), val(J), val(K), val(L), val(M), val(N),

  write(Final), nl,
  
  rule(Final,1),
  rule(Final,2),
  rule(Final,3),
  rule(Final,4),
  rule(Final,5),
  rule(Final,6),
  rule(Final,7). 

  
rule(L,K) :-  count2(L,K),
                nth1(I,L,K),
                J is I + K + 1,
                nth1(J,L,K).
  
  
 count2(L, E) :-
    include(=(E), L, L2), 
    length(L2, 2).
  

val(1). 
val(2).
val(3).
val(4).
val(5).
val(6).
val(7).
  
  