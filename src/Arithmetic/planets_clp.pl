/*
  SATURN
  URANUS
 NEPTUNE
+  PLUTO
---------
 PLANETS
*/

:- use_module(library(clpfd)).

% these are the unique letters in the puzzle
solve([A, E, L, N, O, P, R, S, T, U, Leading]) :-

        %rules of this problem
        S1 #= N + S + E + O,
        S #= S1 mod 10,
        carry(S1,Sc),

        T1 #= R + U + N + T + Sc,
        T #= T1 mod 10,
        carry(T1,Tc),

        E1 #= U + N + U + U + Tc,
        E #= E1 mod 10,
        carry(E1,Ec),
        
        N1 #= T + A + T + L + Ec,
        N #=  N1 mod 10,
        carry(N1,Nc),

        A1 #= A + R + P + P + Nc,
        A #= A1 mod 10,
        carry(A1,Ac),

        L1 #= S + U + E + Ac,
        L #= L1 mod 10,
        carry(L1,Lc),

        P1 #= N + Lc,
        P #= P1 mod 10,
        carry(P1,Leading),

        % constraints
        %all_distinct([A, E, L, N, O, P, R, S, T, U]),
        [A, E, L, N, O, P, R, S, T, U] ins 0..9.
      

carry(S,0) :- S #=< 9.  
carry(S,1) :- S #>= 10, S #=< 19.
carry(S,2) :- S #>= 20, S #=< 29.
carry(S,3) :- S #>= 30, S #=< 39.
carry(S,4) :- S #>= 40, S #=< 49.
carry(S,5) :- S #>= 50, S #=< 59.

go([A, E, L, N, O, P, R, S, T, U, Leading]) :- 
                solve([A, E, L, N, O, P, R, S, T, U, Leading]), 
                label([A, E, L, N, O, P, R, S, T, U, Leading]),
                write([A, E, L, N, O, P, R, S, T, U, Leading]),
                 format("\n\n~w + ~w + ~w + ~w = ~w\n",[[S,A,T,U,R,N],[U,R,A,N,U,S],[N,E,P,T,U,N,E],[P,L,U,T,O],[Leading,P,L,A,N,E,T,S]]).