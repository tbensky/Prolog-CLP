# Learning Constraint Logic Programming with Prolog

This repo is a record of my attempt to research and learn Prolog programming using Constraint Logic Programming (CLP).  The work here was greatly inspired along by the [Power of Prolog](https://www.metalevel.at/prolog/optimization) series, reading throug [this code](https://github.com/triska/clpz/blob/master/clpz.pl), and [the SWI Prolog CLP page](https://www.swi-prolog.org/man/clpfd.html).

CLP is a "new" (mid-2000s) addition to Prolog that didn't exist in Prolog in the late 1980s (when I first dabbled with Prolog). I can (personally) see how CLP greatly enhances the power of logic programming. In short, CLP allows Prolog searches to proceed with incomplete conclusions about the data. In such cases, the old Prolog would simply fail and stop. 

In the case of CLP, Prolog's search happily continues with incomplete conclusions, hoping to firm up such (later), as part of the overall search.

At this point, Prolog + CLP is a bit tough to study. Outside of Bratko, in the 4th edition of ["Prolog Programming for Artificial Intelligence"](https://www.amazon.com/Programming-Artificial-Intelligence-International-Computer/dp/0321417461/) (chapters 7 and 14), there isn't really any books on Prolog + CLP, so there's no unified source for learning or reading about it. (I'm old fashioned too; I learn things best from books.)

It seems best then to just jump in and start experimenting with it, which is what I'm doing here. (Maybe this repo will be a book someday?)


## Project: Basic ideas of CLP

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

Which happily runs, acknowledging that X is 4, Y will be involved in $4+Z=Y$ and Z will be between 3 and 9. If the code continued,
at some point Z might be nailed down, at which point, Prolog will be able to find Y as well.

This is the basic idea of Prolog+CLP. Programs don't have to stop just becase variables are not known. It's happy to work
with *constraints* on variables and continue.

### Labeling

In this first CLP xample, if we run ``go(X,Y,Z), label([X,Y,Z])`` then Prolog will actually start emitting solutions that
meet the given constraints. Here's the output:

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

# Learning Projects

I'm not a puzzle person. I've never done a Sudoku or crossword puzzle. I also never really liked writing code to solve puzzles. But, since [this book](https://www.amazon.com/Art-Computer-Programming-Combinatorial-Algorithms/dp/0201038048/) came out, I became quickly convinced that puzzles are actually a good way of learning CLP. So OK, puzzles it'll be. Let's start with Sudoku.

## Project: Sudoku with and without CLP

Here, we'll study implementing Sudoku in Prolog using two methods, one with and without CLP.

### Without CLP

Solving a full 9x9, with a atomic "guess and check" backtracking Sudoku solver (traditional Prolog), is not feasible. As [Norvig](https://norvig.com/sudoku.html) said:

> First, we could try a brute force approach. Suppose we have a very efficient program that takes only one instruction to evaluate a position, and that we have access to the next-generation computing technology, let's say a 10GHz processor with 1024 cores, and let's say we could afford a million of them, and while we're shopping, let's say we also pick up a time machine and go back 13 billion years to the origin of the universe and start our program running. We can then compute that we'd be almost 1% done with this one puzzle by now.


So, we have to do a 4x4 puzzle instead. Here's the code for it (inspired by the one in [this book](https://www.amazon.com/Thinking-Computation-First-Course-Press/dp/0262534746/)). Our notation here is that columns are labeled A..D and rows are labeled 1..4.

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
% do if the length of sorted and unsorted list are the same, then the list must not have
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

This appears to be the only solution.

### With CLP

We can go for the 9x9 Sudoku solver, using CLP, which is always looking to prune the search space. This code will solve a 9x9 Sudoku almost immediately. Our notation here is that columns are labeled A..I and rows are labeled 1..9.

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


## Project: Crypto-arithmetic (or alphametics) puzzles

There are some famous puzzles like finding values for all variables such that

```
   SEND
+  MORE
--------
  MONEY
```

and

```
   TWO
+  TWO
------
  FOUR
```

The idea is to find values for all variables so that the sums hold. We also have the constrains that there are no leading zeros in the
solution, and all digits of the addends are distinct.

Here, we'll do the TWO+TWO=FOUR puzzle. Looks tricky to do by hand since the "O" is both in the addends and the sum. Our technique can be applied to the SEND+MORE=MONEY problem as well.

### Grade school addition

Remembering how to do these stacked "grade school" addition problems: Start with the rightmost column of digits.  Add the two digits. If the sum is less than 10, we write the sum under the two digits and go
to the next column to the left and repeat. If the sum is ever larger than 10, we write the ones digit under the two digits, and "carry" the one, which means add it to the sum of the next
left column.

In terms of code, we'll do this

* Add two digits. The sum "mod 10" will be written under the two digits.

* Check the sum and see if it's larger than 10. If so, add a 1 to the next column on the left.

* We have a predicate called `carry` that will give us a 1 or 0 to add onto the current column we're working on, based on the sum of the previous (right) column.

This problem also has a constraint that $F\ne 0$.

### Without CLP

Without CLP, we are starting to see a pattern emerge in the Prolog code for these constrained searches. The pattern is 1) assign variables, 2) apply and rules to be followed 3) then apply the constraints. This is outlined on p. 89 of Levesque, "Thinking as Computation." Here, we proceed as follows.  

Another contraint to the pattern ("classic 1980s Prolog") is to be sure all variables are assigned before they are later referenced (which is a much looser constraint in the CLP mode).

First, we allow Prolog to find values for needed variables. These are drawn from some predicate that reflects the domain needed for the variables. Here, T, W, and O are assigned values from the `value` predicate.

Next, more variable assignments are made as per the rules of the problem. In this case, the grade school addition, which will assign values to R, U, O, and F, taking any carries into account.

Lastly, constrains are applied, in this case that T, W, and O are all not equal, and $F\ne 0$.

```prolog

solve([F,O,U,R,T,W,O]) :-

        % assign variables
        value(T),
        value(W),
        value(O),

        %rules of this problem
        R is (O + O) mod 10,
        carry(O + O,Rc),

        U is (W + W + Rc) mod 10,
        carry(W + W + Rc,Uc),

        O is (T + T + Uc) mod 10,
        carry(T + T + Uc,Oc),
        
        % constraints
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
```


### With CLP

Same basic plan and structure as the non-CLP version above.  We instead use `#=` for assignment nad `#\=` for inequality testing. The program structure is a bit different now, since CLP's ``#=`` won't fail if variables are not yet defined. Hence, we can do some variable assignments first, then apply the rules, then set the variable domain later using the ``ins 0..9`` line. We found that the `[T,W,O] ins 0..9.` and `all_distinct([T,W,O])` lines can go anywhere in the `solve` predicate.

```prolog
:- use_module(library(clpfd)).

solve([F,O,U,R,T,W,O]) :-
        R #= (O + O) mod 10,
        carry(O + O,Rc),

        U #= (W + W + Rc) mod 10,
        carry(W + W + Rc,Uc),

        O #= (T + T + Uc) mod 10,
        carry(T + T + Uc,Oc),
        
        F #= Oc, F #\= 0,

        [T,W,O] ins 0..9,
        all_distinct([T,W,O]).

carry(S,1) :- S #>= 10.
carry(S,0).

go([F,O,U,R,T,W,O]) :- solve([F,O,U,R,T,W,O]), label([F,O,U,R,T,W,O]).
```

There seems to be many solutions to this problem, noting the list gives the values of F, O, U, and R, then T, W, then O. (So the first line
below says that 765+765=1530, which is true.)

```prolog
L = [1, 5, 3, 0, 7, 6, 5] ;
L = [1, 5, 7, 0, 7, 8, 5] ;
L = [1, 5, 9, 0, 7, 9, 5] ;
L = [1, 7, 1, 4, 8, 5, 7] ;
L = [1, 7, 3, 4, 8, 6, 7] ;
L = [1, 7, 9, 4, 8, 9, 7] ;
L = [1, 6, 1, 2, 8, 0, 6] ;
L = [1, 6, 1, 2, 8, 5, 6] ;
L = [1, 6, 3, 2, 8, 1, 6] ;
L = [1, 6, 5, 2, 8, 2, 6] ;
L = [1, 6, 5, 2, 8, 7, 6] ;
L = [1, 6, 7, 2, 8, 3, 6] ;
L = [1, 6, 9, 2, 8, 4, 6] ;
L = [1, 6, 9, 2, 8, 9, 6] ;
L = [1, 8, 1, 6, 9, 0, 8] ;
L = [1, 8, 1, 6, 9, 5, 8] ;
L = [1, 8, 3, 6, 9, 1, 8] ;
L = [1, 8, 3, 6, 9, 6, 8] ;
L = [1, 8, 5, 6, 9, 2, 8] ;
L = [1, 8, 5, 6, 9, 7, 8] ;
L = [1, 8, 7, 6, 9, 3, 8] ;
L = [1, 8, 9, 6, 9, 4, 8] ;
L = [1, 1, 2, 2, 5, 6, 1] ;
L = [1, 1, 4, 2, 5, 7, 1] ;
L = [1, 1, 6, 2, 5, 8, 1] ;
L = [1, 1, 8, 2, 5, 9, 1] ;
L = [1, 3, 0, 6, 6, 5, 3] ;
L = [1, 3, 4, 6, 6, 7, 3] ;
L = [1, 3, 6, 6, 6, 8, 3] ;
L = [1, 3, 8, 6, 6, 9, 3] ;
L = [1, 5, 2, 0, 7, 6, 5] ;
L = [1, 5, 6, 0, 7, 8, 5] ;
L = [1, 5, 8, 0, 7, 9, 5] ;
L = [1, 7, 0, 4, 8, 5, 7] ;
L = [1, 7, 2, 4, 8, 6, 7] ;
L = [1, 7, 8, 4, 8, 9, 7] ;
L = [1, 0, 2, 0, 5, 1, 0] ;
L = [1, 0, 2, 0, 5, 6, 0] ;
L = [1, 0, 4, 0, 5, 2, 0] ;
L = [1, 0, 4, 0, 5, 7, 0] ;
L = [1, 0, 6, 0, 5, 3, 0] ;
L = [1, 0, 6, 0, 5, 8, 0] ;
L = [1, 0, 8, 0, 5, 4, 0] ;
L = [1, 0, 8, 0, 5, 9, 0] ;
L = [1, 2, 0, 4, 6, 0, 2] ;
L = [1, 2, 0, 4, 6, 5, 2] ;
L = [1, 2, 2, 4, 6, 1, 2] ;
L = [1, 2, 4, 4, 6, 7, 2] ;
L = [1, 2, 6, 4, 6, 3, 2] ;
L = [1, 2, 6, 4, 6, 8, 2] ;
L = [1, 2, 8, 4, 6, 4, 2] ;
L = [1, 2, 8, 4, 6, 9, 2] ;
L = [1, 4, 0, 8, 7, 0, 4] ;
L = [1, 4, 0, 8, 7, 5, 4] ;
L = [1, 4, 2, 8, 7, 1, 4] ;
L = [1, 4, 2, 8, 7, 6, 4] ;
L = [1, 4, 4, 8, 7, 2, 4] ;
L = [1, 4, 6, 8, 7, 3, 4] ;
L = [1, 4, 6, 8, 7, 8, 4] ;
L = [1, 4, 8, 8, 7, 9, 4] ;
L = [1, 6, 0, 2, 8, 0, 6] ;
L = [1, 6, 0, 2, 8, 5, 6] ;
L = [1, 6, 2, 2, 8, 1, 6] ;
L = [1, 6, 4, 2, 8, 2, 6] ;
L = [1, 6, 4, 2, 8, 7, 6] ;
L = [1, 6, 6, 2, 8, 3, 6] ;
L = [1, 6, 8, 2, 8, 4, 6] ;
L = [1, 6, 8, 2, 8, 9, 6] ;
L = [1, 8, 0, 6, 9, 0, 8] ;
L = [1, 8, 0, 6, 9, 5, 8] ;
L = [1, 8, 2, 6, 9, 1, 8] ;
L = [1, 8, 2, 6, 9, 6, 8] ;
L = [1, 8, 4, 6, 9, 2, 8] ;
L = [1, 8, 4, 6, 9, 7, 8] ;
L = [1, 8, 6, 6, 9, 3, 8] ;
L = [1, 8, 8, 6, 9, 4, 8] ;
```


## Project: One more alphametric

Knuth (4A, p. 346, #24) has a few more of these puzzles proposed. Let's do one more he suggests, which is

```
  SATURN
  URANUS
 NEPTUNE
+  PLUTO
---------
 PLANETS
```


### Without CLP
We immediately notice that in the first column, S=N+S+E+O, which means computing S depends on S itself. The second column has the same issue with T. Thus, a non-CLP Prolog implementation will not work without proposing values for some of these variables first. 

We also have to be prepared for larger carry digits, and 9+9+9+9 (+9 for a carry) is possible. This 9x5=45, so our carry predicate has been extended. We also defined the variable `Leading`, which will be the leading digit of the sum.  P is still constrained to 0..9, but in the leftmost column, there might be a carry to add to N which may make that last sum larger than 10.

### With CLP
Here is our CLP implementation.


```prolog
/*
  SATURN
  URANUS
 NEPTUNE
+  PLUTO
---------
 PLANETS
*/

:- use_module(library(clpfd)).

% these are the unique letters in the puzzle
solve([A, E, L, N, O, P, R, S, T, U, Leading]) :-

        %rules of this problem
        S1 #= N + S + E + O,
        S #= S1 mod 10,
        carry(S1,Sc),

        T1 #= R + U + N + T + Sc,
        T #= T1 mod 10,
        carry(T1,Tc),

        E1 #= U + N + U + U + Tc,
        E #= E1 mod 10,
        carry(E1,Ec),
        
        N1 #= T + A + T + L + Ec,
        N #=  N1 mod 10,
        carry(N1,Nc),

        A1 #= A + R + P + P + Nc,
        A #= A1 mod 10,
        carry(A1,Ac),

        L1 #= S + U + E + Ac,
        L #= L1 mod 10,
        carry(L1,Lc),

        P1 #= N + Lc,
        P #= P1 mod 10,
        carry(P1,Leading),

        % constraints
        %all_distinct([A, E, L, N, O, P, R, S, T, U]),
        [A, E, L, N, O, P, R, S, T, U] ins 0..9.
      

carry(S,0) :- S #=< 9.  
carry(S,1) :- S #>= 10, S #=< 19.
carry(S,2) :- S #>= 20, S #=< 29.
carry(S,3) :- S #>= 30, S #=< 39.
carry(S,4) :- S #>= 40, S #=< 49.
carry(S,5) :- S #>= 50, S #=< 59.

go([A, E, L, N, O, P, R, S, T, U, Leading]) :- 
                solve([A, E, L, N, O, P, R, S, T, U, Leading]), 
                label([A, E, L, N, O, P, R, S, T, U, Leading]),
                write([A, E, L, N, O, P, R, S, T, U, Leading]),
                format("\n\n~w + ~w + ~w + ~w = ~w\n",[[S,A,T,U,R,N],[U,R,A,N,U,S],[N,E,P,T,U,N,E],[P,L,U,T,O],[Leading,P,L,A,N,E,T,S]]).
```

It looks like there's a lot of answers. Here's the first few:

```
[0,0,0,0,0,0] + [0,0,0,0,0,0] + [0,0,0,0,0,0,0] + [0,0,0,0,0] = [0,0,0,0,0,0,0,0]

[7,0,4,3,7,0] + [3,7,0,0,3,7] + [0,0,1,4,3,0,0] + [1,1,3,4,0] = [0,1,1,0,0,0,4,7]

[9,0,3,3,7,0] + [3,7,0,0,3,9] + [0,0,1,3,3,0,0] + [1,3,3,3,0] = [0,1,3,0,0,0,3,9]

[6,1,4,3,7,0] + [3,7,1,0,3,6] + [0,0,1,4,3,0,0] + [1,0,3,4,0] = [0,1,0,1,0,0,4,6]

[8,1,3,3,7,0] + [3,7,1,0,3,8] + [0,0,1,3,3,0,0] + [1,2,3,3,0] = [0,1,2,1,0,0,3,8]

[7,2,3,3,7,0] + [3,7,2,0,3,7] + [0,0,1,3,3,0,0] + [1,1,3,3,0] = [0,1,1,2,0,0,3,7]

[9,2,2,3,7,0] + [3,7,2,0,3,9] + [0,0,1,2,3,0,0] + [1,3,3,2,0] = [0,1,3,2,0,0,2,9]

[6,3,3,3,7,0] + [3,7,3,0,3,6] + [0,0,1,3,3,0,0] + [1,0,3,3,0] = [0,1,0,3,0,0,3,6]

[8,3,2,3,7,0] + [3,7,3,0,3,8] + [0,0,1,2,3,0,0] + [1,2,3,2,0] = [0,1,2,3,0,0,2,8]

[7,4,2,3,7,0] + [3,7,4,0,3,7] + [0,0,1,2,3,0,0] + [1,1,3,2,0] = [0,1,1,4,0,0,2,7]
```

## Project: Langford Pairs


Knuth begins Volume 4A with "Langford Pairs" (They're literally mentioned in sentence #3 on p. 1).  Langford pairs are a way of combining pairs of numbers, like {1,1,2,3,3,...,n,n}.  The combination is to contain all of these numbers in such a way that for a given pair of numbers k (like k=1, so 1 and 1, or k=2, so 2 and 2), k other digits appear between the pair. So, for the pair of 1s, one other digit should appear between the 1 and 1. For the 3, 3 other digits should appear between the two 3s, etc.

As an example, for the set {1,1,2,2,3,3,4,4}, the Langford pairing is {2,3,4,2,1,3,1,4}. The job here is to formulate the search for a Langford pairing of a set of numbers.

This was a tough problems for us to formulate. After coming up with a basic n=4 code below though, things started to make more sense.  The key part of this logic is again, freeing our minds from procedural coding, and realizing that Prolog predicates can be called with any combination of their inputs instantiated.  Here for example, the ```nth1``` predicate can not only retrieve a given list element at some position, but it will also search a list for an element and return its position. 

### Without CLP

We start here by setting up the basic structure into list ```L``` (an 8-element list), then select values for all elements using the ```val``` predicate, which contain the domain needed here (numbers from 1 to 4).

Then we apply to Langford constraints, using blocks like this:

```prolog
nth1(Index11,L,1),
Index12 is Index11 + 2,
nth1(Index12,L,1), 
```

which work as follows. First, find where a 1 appears in list `L` and put its position into `Index11`. Next, make `Index12` point 2 elements away, and look at position `Index12` with the second call to `nth1` and see if it also contains a 1. (Note: looking 2 elements away leaves 1 list element between the two 1s, as needed.)  A block like this appears for all digits 1 to 4.

Lasly, using the `count2` calls, we ensure each digit appears exacty twice in the final Langford list. Here's the code:


```prolog
langford(L) :-

  L = [A,B,C,D,E,F,G,I],
  val(A), val(B), val(C), val(D), val(E), val(F), val(G), val(I),
  
  nth1(Index11,L,1),
  Index12 is Index11 + 2,
  nth1(Index12,L,1), 
    
  nth1(Index21,L,2),
  Index22 is Index21 + 3,
  nth1(Index22,L,2),
    
  nth1(Index31,L,3),
  Index32 is Index31 + 4,
  nth1(Index32,L,3),
  
  nth1(Index41,L,4),
  Index42 is Index41 + 5,
  nth1(Index42,L,4),
    
  count2(L,1),
  count2(L,2),
  count2(L,3),
  count2(L,4).
  
count2(L, E) :-
    include(=(E), L, L2), 
    length(L2, 2).

val(1). 
val(2).
val(3).
val(4).
  
```


The code can be shortened using a predicate called `rule` that enforces the Langford rules, on a list `L` for a number `K`, which is

```prolog
rule(L,K) :-  count2(L,K),
                nth1(I,L,K),
                J is I + K + 1,
                nth1(J,L,K).
               
```

Here, we moved the `count2` to the start of the body, to force `rule` to fail if there aren't 2 of a given number in the list (no reason to do the searching with the `nth1` clauses otherwise).  So, the shorter code becomes:

```prolog
langford(L) :-

    L = [A,B,C,D,E,F,G,I],
    val(A), val(B), val(C), val(D), val(E), val(F), val(G), val(I),

    rule(L,1),
    rule(L,2),
    rule(L,3),
    rule(L,4).


rule(L,K) :-  count2(L,K),
                nth1(I,L,K),
                J is I + K + 1,
                nth1(J,L,K).
               
count2(L, E) :-
    include(=(E), L, L2), 
    length(L2, 2).

val(1). 
val(2).
val(3).
val(4).
```

There are two solutions for n=4, which the computer finds readily in under a second. There is no solution for n=5 or n=6, and 26 for n=7, so let's expand the code above for n=7, which is:

```prolog
langford(Final) :-

  Final = [A,B,C,D,E,F,G,H,I,J,K,L,M,N],
  
  val(A), val(B), val(C), val(D), val(E), val(F), val(G), 
  val(H), val(I), val(J), val(K), val(L), val(M), val(N),
  
  rule(Final,1),
  rule(Final,2),
  rule(Final,3),
  rule(Final,4),
  rule(Final,5),
  rule(Final,6),
  rule(Final,7). 

  
rule(L,K) :-  count2(L,K),
                nth1(I,L,K),
                J is I + K + 1,
                nth1(J,L,K).
  
  
 count2(L, E) :-
    include(=(E), L, L2), 
    length(L2, 2).
  

val(1). 
val(2).
val(3).
val(4).
val(5).
val(6).
val(7).
```

What we notice right away that we have a "real" combinatorics problem here: the computer can't seem to find a Langford solution for n=7. The search space is too large. 

We investigated a bit and found that even if we change the `rule(L,K)` body to `rule(L,K) :- count2(L,K).`, in other words, to just look for sequences where each digit appears twice, the computer still cannot even find one of these in any short amount of time. This tell us: never mind the Langford spacings, we need to get better at generating test sequences alone, that just have each digit appear twice. Our search algorithm is too random right now.

#### A more "intelligent" domain

The domain selection above isn't very helpful.  We essentially tell Prolog to get a number from 1..7, and try to place it (and others) to meet the Langford rules. It's essentially a random search.  What if we got more clever about the domain selection to guide Prolog to the *Langford* sequence we seek?

Here, make our domain a little more restrictive at the onset. Instead of choosing a number, let's choose a number and pre-calculate the two positions in the final solution the number must appear. For example, if we choose a 1, we know it has to apppear at index $n_1$ and index $n_1+1+1$. Or, more generally, number $k$ has to appear at index $n_1$ and $n_1+k+1$.  This is what the `domain(Len,K,N1,N2)` clause does.

It assumes it has `Len` spots to fill in the Langford sequence. (Here spot also means index or position in the Langford sequence.) It finds a number `K` from `val(K)` then chooses a possible index for it (in the Langford sequence). We note the domain of `aindex` is always `1..Len-2`, because at minimum a `1_1` sequence might be squeezed into the last 3 positions.

So, `domain` instantiates a number, `K` and the two positions in the Langford sequence it is proposed to go (`N1` and `N2`).

We are maintaining our Langford sequence in a list of lists.  Each sub-list has 3 elements: the number and two positions it should appear. So if a sub-list is `[1,2,4]`, this means `1` should appear at position `2` and `4`.

The return values returned from `domain` need to be checked using the `member` sequences.

* Make sure the number `K` hasn't already been placd: `\+ member([K,_,_],L0)`
* Make sure position `N1` has not already been used by some other number: `\+ member([_,N1,_],L0)` and ` \+ member([_,_,N1],L0)`
* Make sure position `N2` has not already been used by some other number: ` \+ member([_,_,N2],L0)` and `  \+ member([_,N2,_],L0)`

Then we call `langford` recursively using `langford([[K,N1,N2]|L0],Soln)`. This prepends the sub-list that passes all Langford tests to
running list `L0` via the `[[K,N1,N2]|L0]` construct, and hangs onto variable `Soln` to present the final solution. The terminal case
is when the Langford sub-lists reach a length of the maximum number in the set. (Recall each sub-lists holds the number and its two
positions.)

Here's this approach for the ${11,22,33,44}$ set. It can be called with:

```prolog
langford([],L).
```

```prolog
langford(L,L) :- length(L,4).

langford(L0,Soln) :-
            domain(8,K,N1,N2),
        
            \+ member([K,_,_],L0),
            \+ member([_,N1,_],L0),
            \+ member([_,_,N2],L0),
            \+ member([_,N2,_],L0),
            \+ member([_,_,N1],L0),

            langford([[K,N1,N2]|L0],Soln).


domain(Len,K,N1,N2) :-
        val(K),
        aindex(N1),
        N1max is Len - (K + 1), 
        N1 =< N1max,
        N2 is N1 + (K + 1).


val(1).
val(2).
val(3).
val(4).

aindex(1).
aindex(2).
aindex(3).
aindex(4).
aindex(5).
aindex(6).
```

For the ${1,1,2,2,3,3,4,4,5,5,6,6,7,7}$ set, the following gives a solution instantly, by calling:

```prolog
langford([],L).
```

One will get:

```
L = [[7, 2, 10], [6, 6, 13], [5, 5, 11], [4, 9, 14], [3, 8, 12], [2, 4, 7], [1, 1, 3]] 
```

telling us that 7 goes into places 2 and 10, 6 goes into places 6 and 13, 5 into 5 and 11, etc.

Here's another solution:

```
L = [[7, 2, 10], [5, 8, 14], [3, 9, 13], [6, 5, 12], [4, 6, 11], [2, 4, 7], [1, 1, 3]] 
```

```prolog
langford(L,L) :- length(L,7).

langford(L0,Soln) :-
            domain(14,K,N1,N2),
        
            \+ member([K,_,_],L0),
            \+ member([_,N1,_],L0),
            \+ member([_,_,N2],L0),
            \+ member([_,N2,_],L0),
            \+ member([_,_,N1],L0),

            langford([[K,N1,N2]|L0],Soln).


domain(Len,K,N1,N2) :-
        val(K),
        aindex(N1),
        N1max is Len - (K + 1), 
        N1 =< N1max,
        N2 is N1 + (K + 1).


val(1).
val(2).
val(3).
val(4).
val(5).
val(6).
val(7).

aindex(1).
aindex(2).
aindex(3).
aindex(4).
aindex(5).
aindex(6).
aindex(7).
aindex(8).
aindex(9).
aindex(10).
aindex(11).
aindex(12).
```

