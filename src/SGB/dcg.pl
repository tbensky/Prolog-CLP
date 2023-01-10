s --> [a], [b].
s --> [a], s, [b].

ss([a|List],Rest) :- s(List,[b|Rest]).
