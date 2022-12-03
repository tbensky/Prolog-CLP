

/*

4x4 sudoku

R11 R12 R13 R14
R21 R22 R23 R24
R31 R32 R33 R34
R41 R42 R43 R44


*/


sudoku(R11,R12,R13,R14,R21,R22,R23,R24,R31,R32,R33,R34,R41,R42,R43,R44) :-
   solution(R11,R12,R13,R14,R21,R22,R23,R24,R31,R32,R33,R34,R41,R42,R43,R44),
   nl, write('A solution to this puzzle is'), nl,
   printrow(R11,R12,R13,R14), printrow(R21,R22,R23,R24),
   printrow(R31,R32,R33,R34), printrow(R41,R42,R43,R44).

printrow(P,Q,R,S) :- write(' '), write(P), write(' '), write(Q),
   write(' '), write(R), write(' '), write(S), nl.


solution(R11,R12,R13,R14,R21,R22,R23,R24,R31,R32,R33,R34,R41,R42,R43,R44) :-
                              %rows
                              uniq(R11,R12,R13,R14), uniq(R21,R22,R23,R24),
                              uniq(R31,R32,R33,R34), uniq(R41,R42,R43,R44),

                              %columns
                              uniq(R11,R21,R31,R41), uniq(R12,R22,R32,R42),
                              uniq(R13,R23,R33,R43), uniq(R14,R24,R34,R44),

                              %blocks
                              uniq(R11,R12,R21,R22), uniq(R13,R14,R23,R24),
                              uniq(R31,R32,R41,R42), uniq(R33,R34,R43,R44).

uniq(P,Q,R,S) :- num(P),  num(Q),  num(R),  num(S),
                 \+ P=Q, \+ P=R, \+ P=S, \+ Q=R, \+ Q=S, \+ R=S.


num(1).  
num(2).  
num(3).  
num(4).

go :- sudoku(
   1,4,_,_,
   _,_,4,_,
   2,_,_,_,
   _,_,_,3
   ).





