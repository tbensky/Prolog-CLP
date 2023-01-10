gen_ab(List,Rest) :- ab(List,Rest).


gen_ab(List1,Rest) :- 
                    ab(List1,List2),
                    gen_ab(List2,Rest).


ab([a|Rest],Rest).
ab([b|Rest],Rest).

xy([R],R).
cd(a,a).



pal_term([],_).
pal_term([_|Rest],Rest).
pal_term([X|Rest])