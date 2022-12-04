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

Solving a full 9x9, with a atomic "guess and check" backtracking Sudoku solver (traditional Prolog), is not feasible. As [Norvig](https://norvig.com/sudoku.html) said:

> First, we could try a brute force approach. Suppose we have a very efficient program that takes only one instruction to evaluate a position, and that we have access to the next-generation computing technology, let's say a 10GHz processor with 1024 cores, and let's say we could afford a million of them, and while we're shopping, let's say we also pick up a time machine and go back 13 billion years to the origin of the universe and start our program running. We can then compute that we'd be almost 1% done with this one puzzle by now.


So, we have to do a 4x4 puzzle instead. Here's the code for it (inspired by the one in [this book](https://www.amazon.com/Thinking-Computation-First-Course-Press/dp/0262534746/)):

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

is displayed, and this appears to be the only solution.

### With CLP

We can go for the 9x9 Sudoku solver, using CLP, which is always looking to prune the search space. This will solve a 9x9 Sudoku almost immediately.

```Prolog
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


```

Note the structure is very similar to the non-CLP Sudoko: set up a board, assign elements to variables, and apply the Sudoku rules (unique rows, columns, and now 4x4 blocks). 

The difference now is two-fold. First, the `all_distinct` predicate is part of the CLPFD library. It stores the fact that we want each variable in a given list to have a unique value. 

Second is the `X ins 1..9`. The `ins` predicate is also part of the CLPFD library. It sets the domain of each variable in the list `X` to be within 1 to 9 (`ins` is a clever help that maps the domain needs over all elements of a list. It is similar to the `maplist(domain,...)` call in the non-CLP solver).

The following solution is produced.

```
[1,5,6,8,9,4,3,2,7]
[9,2,8,7,3,1,4,5,6]
[4,7,3,2,6,5,9,1,8]
[3,6,2,4,1,7,8,9,5]
[7,8,9,3,5,2,6,4,1]
[5,1,4,9,8,6,2,7,3]
[8,3,1,5,4,9,7,6,2]
[6,9,7,1,2,3,5,8,4]
[2,4,5,6,7,8,1,3,9]
```