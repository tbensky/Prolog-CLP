/*

4x4 sudoku

A1 B1 C1 D1
A2 B2 C2 D2
A3 B3 C3 D3
A4 B4 C4 D4

*/


sudoku([
         A1, B1, C1, D1,
         A2, B2, C2, D2,
         A3, B3, C3, D3,
         A4, B4, C4, D4
         ] ) :-

            
         maplist(domain,[
                           A1, B1, C1, D1,
                           A2, B2, C2, D2,
                           A3, B3, C3, D3,
                           A4, B4, C4, D4
                           ]),
     
         %columns
         distinct([A1,A2,A3,A4]),
         distinct([B1,B2,B3,B4]),
         distinct([C1,C2,C3,C4]),
         distinct([D1,D2,D3,D4]),

         %rows
         distinct([A1,B1,C1,D1]),
         distinct([A2,B2,C2,D2]),
         distinct([A3,B3,C3,D3]),
         distinct([A4,B4,C4,D4]),

         %blocks
         distinct([A1,B1,A2,B2]),
         distinct([A3,B3,A4,B4]),
         distinct([C1,D1,C2,D2]),
         distinct([C3,D3,C4,D4]).

         
   

% sort removes duplicate elements upon the sort, so [4,3,3,2,1] will become [1,2,3,4].
% do if length of sorted and unsorted list are the same, then the list must not have
% duplicate elements.
distinct(L) :- sort(L,L1), length(L,Len), length(L1,Len).

 
%domains for the numbers
domain(1).  
domain(2).  
domain(3).  
domain(4).


print_soln([]) :- nl.
print_soln([A,B,C,D|T]) :- write([A,B,C,D]), nl, print_soln(T). 


go(X) :- X = [
               1,4,_,_,
               _,_,4,_,
               2,_,_,_,
               _,_,_,3
            ],

         sudoku(X),
         print_soln(X).





