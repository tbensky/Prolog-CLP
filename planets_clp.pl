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
solve([A, E, L, N, O, P, R, S, T, U]) :-

        %rules of this problem
        S #= (N + S + E + O) mod 10,
        carry(N + S + E + O,Sc),

        T #= (R + U + N + T) mod 10,
        carry(R + U + N + T + Sc,Tc),

        E #= (U + N + U + Tc) mod 10,
        carry(U + N + U + U + Tc,Ec),
        
        N #= (T + A + T + L + Ec) mod 10,
        carry(T + A + T + L + Ec,Nc),

        A #= (A + R + P + P + Nc),
        carry(A + R + P + P + Nc,Ac),

        L #= (S + U + E + Ac) mod 10,
        carry(S + U + E + Ac,Lc),

        P #= (N + Lc) mod 10,

        % constraints
        %all_distinct([A, E, L, N, O, P, R, S, T, U]),
        [A, E, L, N, O, P, R, S, T, U] ins 0..9.

        

carry(S,1) :- S #>= 10.
carry(S,0).

go([A, E, L, N, O, P, R, S, T, U]) :- solve([A, E, L, N, O, P, R, S, T, U]), label([A, E, L, N, O, P, R, S, T, U]), write([A, E, L, N, O, P, R, S, T, U]).