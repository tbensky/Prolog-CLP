order_ok([]).
order_ok(L) :- length(L,1).
order_ok([A,B|Tail]) :- is_behind(A,B), order_ok([B|Tail]).


% answer: [prancer,cupid,rudolph,dasher,blitzen,vixen,comet,donder,dancer]
% in reverse: [dancer,donder,comet,vixen,blitzen,dasher,rudolph,cupid,prancer]

go(L,L) :-
            length(L,9),
            order_ok(L).


go(L,Soln) :-

    rd(X),
    \+ member(X,L),
    append([X],L,L1),
    go(L1,Soln).
   

% Allows us to only need the is_behind clause
is_behind(X,Y) :- behind(X,Y).
is_behind(X,Y) :- front(Y,X).


rd(vixen).
rd(dancer).
rd(comet).
rd(donder).
rd(cupid).
rd(prancer).
rd(rudolph).
rd(dasher).
rd(blitzen).

% Vixen should be behind Rudolph, Prancer and Dasher,
behind(vixen,rudolph).
behind(vixen,prancer).
behind(vixen,dasher).

% Dancer should be behind Donder, Blitzen and Rudolph. 
behind(dancer,donder).
behind(dancer,blitzen).
behind(dancer,rudolph).

% Comet should be behind Cupid, Prancer and Rudolph.
behind(comet,cupid).
behind(comet,prancer).
behind(comet,rudolph).

% Donder should be behind Comet, Vixen, Dasher, Prancer and Cupid. 
behind(donder,comet).
behind(donder,vixen).
behind(donder,dasher).
behind(donder,prancer).
behind(donder,cupid).

% Blitzen should be behind Cupid 
behind(blitzen,cupid).

% Rudolph should be behind Prancer
behind(rudolph,prancer).

% Finally, Dasher should be behind Prancer
behind(dasher,prancer).

% Cupid should be in front of Comet, Blitzen, Vixen, Dancer and Rudolph
front(cupid,comet).
front(cupid,blitzen).
front(cupid,vixen).
front(cupid,dancer).
front(cupid,rudolph).

% Vixen should be in front of Dancer and Comet.
front(vixen,dancer).
front(vixen,comet).

% Prancer should be in front of Blitzen, Donder and Cupid.
front(prancer,blitzen).
front(prancer,donder).
front(prancer,cupid).


% but in front of Dancer, Vixen and Donder.
front(blitzen,dancer).
front(blitzen,vixen).
front(blitzen,donder).

% but in front of Dasher, Dancer and Donder.
front(rudolph,dasher).
front(rudolph,dancer).
front(rudolph,donder).


% but in front of Blitzen, Dancer and Vixen.
front(dasher,blitzen).
front(dasher,dancer).
front(dasher,vixen).
   

        