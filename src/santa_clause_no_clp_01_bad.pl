go(L) :-

    L = [Dancer,Prancer,Donder,Blitzen,Vixen,Cupid,Comet,Dasher,Rudolph],
    maplist(order,L),

    write(L), nl, sleep(1),

    % Vixen should be behind Rudolph, Prancer and Dasher,
    maplist(>(Vixen),[Rudolph,Prancer,Dasher]),

    %  Vixen should be in front of Dancer and Comet.
    maplist(<(Vixen),[Dancer,Comet]),

    % Dancer should be behind Donder, Blitzen and Rudolph. 
    maplist(>(Dancer),[Donder,Blitzen,Rudolph]),

    % Comet should be behind Cupid, Prancer and Rudolph.
    maplist(>(Comet),[Cupid,Prancer,Rudolph]),

    % Donder should be behind Comet, Vixen, Dasher, Prancer and Cupid. 
    maplist(>(Donder),[Comet, Vixen, Dasher, Prancer,Cupid]),

    % Cupid should be in front of Comet, Blitzen, Vixen, Dancer and Rudolph
    maplist(<(Cupid),[Comet, Blitzen, Vixen, Dancer,Rudolph]),

    % Prancer should be in front of Blitzen, Donder and Cupid.
    maplist(<(Prancer),[Blitzen,Donder,Cupid]),

    % Blitzen should be behind Cupid 
    Blitzen > Cupid,

    % but in front of Dancer, Vixen and Donder.
    maplist(<(Blitzen),[Dancer,Vixen,Donder]),

    % Rudolph should be behind Prancer
    Rudolph > Prancer,

    % but in front of Dasher, Dancer and Donder.
    maplist(<(Rudolph),[Dasher,Dancer,Donder]),

    % Finally, Dasher should be behind Prancer
    Dasher > Prancer,

    % but in front of Blitzen, Dancer and Vixen.
    maplist(<(Dasher),[Blitzen,Dancer,Vixen]).
        


order(1).
order(2).
order(3).
order(4).
order(5).
order(6).
order(7).
order(8).
order(9).

        