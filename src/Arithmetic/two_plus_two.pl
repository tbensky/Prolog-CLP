:- use_module(library(clpfd)).

solve([F,O,U,R,T,W,O]) :-
        all_distinct([T,W,O]),

         [T,W,O] ins 0..9,

        R #= (O + O) mod 10,
        carry(O + O,Rc),

        U #= (W + W + Rc) mod 10,
        carry(W + W + Rc,Uc),

        O #= (T + T + Uc) mod 10,
        carry(T + T + Uc,Oc),
        
        F #= Oc, F #\= 0.

       
     

carry(S,1) :- S #>= 10.
carry(S,0).

go([F,O,U,R,T,W,O]) :- solve([F,O,U,R,T,W,O]), label([F,O,U,R,T,W,O]).