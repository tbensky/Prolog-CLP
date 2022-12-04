:- use_module(library(clpfd)).
:- use_module(library(apply)).

print_ans([]).
print_ans([A,B,C,D,E,F,G,H,I|T]) :- write([A,B,C,D,E,F,G,H,I]), nl, print_ans(T). 

go :-
	sudoku(X),
	rules(X),
	X ins 1..9,
	print_ans(X).


sudoku([
			1,_,_,8,_,4,_,_,_,
            _,2,_,_,_,_,4,5,6,
            _,_,3,2,_,5,_,_,_,
            _,_,_,4,_,_,8,_,5,
            7,8,9,_,5,_,_,_,_,
            _,_,_,_,_,6,2,_,3,
            8,_,1,_,_,_,7,_,_,
            _,_,_,1,2,3,_,8,_,
            2,_,5,_,_,_,_,_,9
		]).

rules([
		A1,B1,C1,D1,E1,F1,G1,H1,I1,
		A2,B2,C2,D2,E2,F2,G2,H2,I2,
		A3,B3,C3,D3,E3,F3,G3,H3,I3,
		A4,B4,C4,D4,E4,F4,G4,H4,I4,
		A5,B5,C5,D5,E5,F5,G5,H5,I5,
		A6,B6,C6,D6,E6,F6,G6,H6,I6,
		A7,B7,C7,D7,E7,F7,G7,H7,I7,
		A8,B8,C8,D8,E8,F8,G8,H8,I8,
		A9,B9,C9,D9,E9,F9,G9,H9,I9
	]) :-

				% cols
				all_distinct([A1,A2,A3,A4,A5,A6,A7,A8,A9]),
				all_distinct([B1,B2,B3,B4,B5,B6,B7,B8,B9]),
				all_distinct([C1,C2,C3,C4,C5,C6,C7,C8,C9]),
				all_distinct([D1,D2,D3,D4,D5,D6,D7,D8,D9]),
				all_distinct([E1,E2,E3,E4,E5,E6,E7,E8,E9]),
				all_distinct([F1,F2,F3,F4,F5,F6,F7,F8,F9]),
				all_distinct([G1,G2,G3,G4,G5,G6,G7,G8,G9]),
				all_distinct([H1,H2,H3,H4,H5,H6,H7,H8,H9]),
				all_distinct([I1,I2,I3,I4,I5,I6,I7,I8,I9]),

				% rows
				all_distinct([A1,B1,C1,D1,E1,F1,G1,H1,I1]),
				all_distinct([A2,B2,C2,D2,E2,F2,G2,H2,I2]),
				all_distinct([A3,B3,C3,D3,E3,F3,G3,H3,I3]),
				all_distinct([A4,B4,C4,D4,E4,F4,G4,H4,I4]),
				all_distinct([A5,B5,C5,D5,E5,F5,G5,H5,I5]),
				all_distinct([A6,B6,C6,D6,E6,F6,G6,H6,I6]),
				all_distinct([A7,B7,C7,D7,E7,F7,G7,H7,I7]),
				all_distinct([A8,B8,C8,D8,E8,F8,G8,H8,I8]),
				all_distinct([A9,B9,C9,D9,E9,F9,G9,H9,I9]),

				% blocks
				all_distinct([A1,B1,C1,A2,B2,C2,A3,B3,C3]),
				all_distinct([A4,B4,C4,A5,B5,C5,A6,B6,C6]),
				all_distinct([A7,B7,C7,A8,B8,C8,A9,B9,C9]),
				all_distinct([D1,E1,F1,D2,E2,F2,D3,E3,F3]),
				all_distinct([D4,E4,F4,D5,E5,F5,D6,E6,F6]),
				all_distinct([D7,E7,F7,D8,E8,F8,D9,E9,F9]),
				all_distinct([G1,H1,I1,G2,H2,I2,G3,H3,I3]),
				all_distinct([G4,H4,I4,G5,H5,I5,G6,H6,I6]),
				all_distinct([G7,H7,I7,G8,H8,I8,G9,H9,I9]).
