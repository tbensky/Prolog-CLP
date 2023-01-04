


add_rd([],X,[X]).
add_rd(L,X,[X|L]).

go(L,L) :- length(L,9).

go(L,Soln) :-
    rd(X),
    add_rd(L,X,L1),
    go(L1,Soln).



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
   

        