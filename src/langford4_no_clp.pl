langford(L) :-
  L = [A,B,C,D,E,F,G,I],
  val(A), val(B), val(C), val(D), val(E), val(F), val(G), val(I),
  
  nth1(Index11,L,1),
  Index12 is Index11 + 2,
  nth1(Index12,L,1), 
    
  nth1(Index21,L,2),
  Index22 is Index21 + 3,
  nth1(Index22,L,2),
    
  nth1(Index31,L,3),
  Index32 is Index31 + 4,
  nth1(Index32,L,3),
  
  nth1(Index41,L,4),
  Index42 is Index41 + 5,
  nth1(Index42,L,4),
    
  count2(L,1),
  count2(L,2),
  count2(L,3),
  count2(L,4).
  
 count2(L, E) :-
    include(=(E), L, L2), 
    length(L2, 2).
  

val(1). 
val(2).
val(3).
val(4).
  
% goal: langford(L), write(L).
