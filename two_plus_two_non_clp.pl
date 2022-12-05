
solve([F,O,U,R,T,W,O]) :-

        value(T),
        value(W),
        value(O),

        R is (O + O) mod 10,
        carry(O + O,Rc),

        U is (W + W + Rc) mod 10,
        carry(W + W + Rc,Uc),

        O is (T + T + Uc) mod 10,
        carry(T + T + Uc,Oc),
        
        F is Oc, F =\= 0,

        T =\= W, T =\= O,
        W =\= O.

carry(S,1) :- S >= 10.
carry(S,0).


value(0).
value(1).
value(2).
value(3).
value(4).
value(5).
value(6).
value(7).
value(8).
value(9).

go([F,O,U,R,T,W,O]) :- solve([F,O,U,R,T,W,O]).