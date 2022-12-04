# Learning Constraint Logic Programming with Prolog

This repo is a record of my attempt to research and learn Prolog programming using Constraint Logic Programming (CLP).  The work here was greatly helped along by the [Power of Prolog](https://www.metalevel.at/prolog/optimization) series.

CLP is a "new" addition to Prolog that didn't exist in Prolog as it existed in the late 1980s.


## Project: Basic ideas

Here is a simple Prolog program that tries to instantiate the variables `X`, `Y`, and `Z`.  

```prolog
go(X,Y,Z) :- 
            X is 4, 
            Y is X + Z, 
            Z > 2, 
            Z < 10.
```

If this is run, we'll get:

```
?- go(X,Y,Z).
ERROR: is/2: Arguments are not sufficiently instantiated
```

It cannot run, because in the line `Y is X + Z`, `Z` has not been set, so the code fails at that line. But supposing in
some logic you are working on, `Z` will be determined, but just later on. Does the code really have to fail already?

With CLP, we can recast the code like this:

```prolog
:- use_module(library(clpfd)).


go(X,Y,Z) :- 
            X #= 4, 
            Y #= X + Z,  
            Z #> 2,
            Z #< 10.
```

Here, the traditional `is` in Prolog is replaced with `#=`, which is the CLP version of assignment. If this code is run, we get

```prolog
X = 4,
Y in 7..13,
4+Z#=Y,
Z in 3..9.
```

Which happily ran, acknowledging that X is 4, Y will be involved in $4+Z=Y$ and Z will be between 3 and 9. If the code continued,
at some point Z might be nailed down, at which point, Prolog will be able to find Y as well.

This is the basic idea of Prolog+CLP. Programs don't have to stop just becase variables are not known. It's happy to work
with *constraints* on variables and continue.

In the last example, if we run ``go(X,Y,Z), label([X,Y,Z])`` then Prolog will actually start spitting out solutions that
meet our constraints, like

```prolog
?- go(X,Y,Z), label([X,Y,Z]).
X = 4,
Y = 7,
Z = 3 ;

X = Z, Z = 4,
Y = 8 ;

X = 4,
Y = 9,
Z = 5 ;

X = 4,
Y = 10,
Z = 6 ;

X = 4,
Y = 11,
Z = 7 ;

X = 4,
Y = 12,
Z = 8 ;

X = 4,
Y = 13,
Z = 9.
```

In CLP, *labeling* is when we force Prolog to come up with actual values for variables.

## Project: Sudoku with and without CLP

I never liked writing code to solve puzzles, but since [this book](https://www.amazon.com/Art-Computer-Programming-Combinatorial-Algorithms/dp/0201038048/) came out, I became quickly convinced that puzzles are actually a good way of learning CLP. So OK, puzzles it'll be. Let's start with Sudoku.

### Without CLP

Solving a full 9x9, with a atomic "guess and check" backtracking solver (traditional Prolog), is not feasible. So, we have to do a 4x4 puzzle. Here's the code for it:

```prolog
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
```

Down in the ```go``` pedicate, we initialize a Sudoku board as needed into variable ```X```. Then we make a call to the solver in ```sudoku(X)```. The header

```prolog
sudoku([
         A1, B1, C1, D1,
         A2, B2, C2, D2,
         A3, B3, C3, D3,
         A4, B4, C4, D4
         ] )
```

maps the incoming Sudoku board into discrete variables.

Next, the code

```prolog
 maplist(domain,[
                           A1, B1, C1, D1,
                           A2, B2, C2, D2,
                           A3, B3, C3, D3,
                           A4, B4, C4, D4
                           ]),
```

chooses a value for each variable by applying each to the `domain` goal.  The Sudoku rules of 1) unique columns, 2) unique rows, and 3) unique 2x2 blocks are enforced in the series of `distinct` predicate calls. We let it go using `go(X).`, and sure enough after about 3 seconds (3 GHz Intel iMac), we'll get the solution of:

```prolog
[1,4,3,2]
[3,2,4,1]
[2,3,1,4]
[4,1,2,3]
```

### With CLP

```Prolog

```