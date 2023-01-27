# Learning Constraint Logic Programming with Prolog

This repo is a record of my attempt to research and learn Prolog programming using Constraint Logic Programming (CLP).  The work here was greatly inspired along by the [Power of Prolog](https://www.metalevel.at/prolog/optimization) series, reading through [this](https://github.com/triska/clpz/blob/master/clpz.pl), and [the SWI Prolog CLP page](https://www.swi-prolog.org/man/clpfd.html).

CLP is a "new" (mid-2000s) addition to Prolog that didn't exist in Prolog in the late 1980s (when I first dabbled with Prolog). I can (personally) see how CLP greatly enhances the power of logic programming. In short, CLP allows Prolog searches to proceed with incomplete conclusions about the data. In such cases, the old Prolog would simply fail and stop. 

In the case of CLP, Prolog's search happily continues with incomplete conclusions, hoping to firm up such (later), as part of the overall search.

At this point, Prolog + CLP is a bit tough to study. I can only find a few solid references on it:

* Bratko, in the 4th edition of ["Prolog Programming for Artificial Intelligence"](https://www.amazon.com/Programming-Artificial-Intelligence-International-Computer/dp/0321417461/) (chapters 7 and 14)
* ["CLP(FD) Constraint Logic Programming over Finite Domains"](https://github.com/Anniepoo/swiplclpfd/blob/master/clpfd.adoc) 
* ["Constraint Processing](https://www.amazon.com/Constraint-Processing-Kaufmann-Artificial-Intelligence/dp/1558608907) by Dechter, Ch 15.


So, it seems best then to just jump in and start experimenting with it, which is what I'll do here. (Maybe this repo will be a book someday?) This is defintely a "learn Prolog the hard way" kind of tutorial.

# Contents

* [Basic ideas of CLP](#project-basic-ideas-of-clp)
* [Projects](#learning-projects)
   * [Sudoku](#project-sudoku-with-and-without-clp)
   * [Arithmetics](#project-crypto-arithmetic-or-alphametics-puzzles)
   * [Langford pairs](#project-langford-pairs)
   * [Santa's Reindeer](#solving-santas-reindeer)
   * [Knuth's Stanford GraphBase](#knuths-stanford-graphbase-five-letter-words)


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

As another example, you may have heard of, is a funny holiday logic-puzzle:

> Vixen should be behind Rudolph, Prancer and Dasher, whilst Vixen should be in front of Dancer and Comet. Dancer should be ...

The problem says something like "find the order of Santa's reindeer." 

It might be good to somehow enumerate all reindeer names, so we can get order numbers for all of them.  Then, we can start interpreting the puzzle, pushing to find
numerical values for each reindeer.

"Vixen should be behind Rudolph, Prancer and Dasher" might mean whatever order Vixen ends up being in, it should a larger number than that of "Rudolph, Prancer and Dasher."  So we might write

```
Vixen > Rudolph
Vixen > Prancer
Vixen > Dasher
```

Next, we look at "Vixen should be in front of Dancer and Comet" for which we might write:

```
Vixen < Dancer
Vixen < Comet
```

Now this is all very nice: whatever order Vixen ends up being in, it'll be a number bigger than that for Rudolph, Prancer, and Dasher, but less than that for Dancer and Comet.  

But we're stuck now, because we can't say anything more about Vixen until we can "solve" for the ordering of the other reindeer.  We can *choose* some values for Rudolph, Prancer, and Dasher, but what values? Finding these values is the whole point of the code here.  But OK, suppose we we choose 5,6, and 7, so then Vixen can be 4.  But we're just guessing; likely these values won't hold up later when other ordering rules are enforced.

At this level, classical (i.e. non-CLP) Prolog has no "magic" to offer. We have to code up a search strategy, since all we can do is guess values to keep the code going. In classical Prolog, we can't just tell it `Vixen > Rudolph`, as it will fail since it doesn't know anything about Rudolph. (Note: this puzzle can indeed be solved with classical Prolog, just not as directly has stated). Using Prolog+CLP however, we can just state

```
Vixen #< Dancer
```

as a *constraint* to be resolved later on. Prolog will happily continue, and if enough constraints are put forth, will provide a solution. This problem is solved below with both a CLP and non-CLP approach.


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

I'm not a puzzle person. I've never done a Sudoku or crossword puzzle (I don't have the patience). I also never really liked *writing code* to solve puzzles. But, since [this book](https://www.amazon.com/Art-Computer-Programming-Combinatorial-Algorithms/dp/0201038048/) came out, I became quickly convinced that puzzles are actually a good way of learning CLP. So OK, puzzles it'll be. Let's start with Sudoku.

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


### Project: One more alphametric

Knuth (4A, p. 346, #24) has a few more of these puzzles proposed. Let's do one more he suggests, which is

```
  SATURN
  URANUS
 NEPTUNE
+  PLUTO
---------
 PLANETS
```


#### Without CLP
We immediately notice that in the first column, S=N+S+E+O, which means computing S depends on S itself. The second column has the same issue with T. Thus, a non-CLP Prolog implementation will not work without proposing values for some of these variables first. 

We also have to be prepared for larger carry digits, and 9+9+9+9 (+9 for a carry) is possible. This 9x5=45, so our carry predicate has been extended. We also defined the variable `Leading`, which will be the leading digit of the sum.  P is still constrained to 0..9, but in the leftmost column, there might be a carry to add to N which may make that last sum larger than 10.

#### With CLP
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

As an example, for the set {1,1,2,2,3,3,4,4}, the Langford pairing is {2,3,4,2,1,3,1,4}. The job here is to formulate the search for a Langford pairing of a set of numbers. Here's an [interesting write-up](https://dialectrix.com/langford/OnLangfordsProblem.pdf).

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

We investigated a bit and found that even if we change the `rule(L,K)` body to `rule(L,K) :- count2(L,K).`, in other words, to just look for sequences where each digit appears twice, the computer still cannot even find one of these in any short amount of time. 

Inserting a `write` statement revealed the search is merely iterating through all possible 14-digit values. Here are the first few test solutions:

```
[1,1,1,1,1,1,1,1,1,1,1,1,1,1]
[1,1,1,1,1,1,1,1,1,1,1,1,1,2]
[1,1,1,1,1,1,1,1,1,1,1,1,1,3]
[1,1,1,1,1,1,1,1,1,1,1,1,1,4]
[1,1,1,1,1,1,1,1,1,1,1,1,1,5]
[1,1,1,1,1,1,1,1,1,1,1,1,1,6]
[1,1,1,1,1,1,1,1,1,1,1,1,1,7]
[1,1,1,1,1,1,1,1,1,1,1,1,2,1]
[1,1,1,1,1,1,1,1,1,1,1,1,2,2]
[1,1,1,1,1,1,1,1,1,1,1,1,2,3]
[1,1,1,1,1,1,1,1,1,1,1,1,2,4]
[1,1,1,1,1,1,1,1,1,1,1,1,2,5]
[1,1,1,1,1,1,1,1,1,1,1,1,2,6]
[1,1,1,1,1,1,1,1,1,1,1,1,2,7]
[1,1,1,1,1,1,1,1,1,1,1,1,3,1]
[1,1,1,1,1,1,1,1,1,1,1,1,3,2]
[1,1,1,1,1,1,1,1,1,1,1,1,3,3]
[1,1,1,1,1,1,1,1,1,1,1,1,3,4]
[1,1,1,1,1,1,1,1,1,1,1,1,3,5]
[1,1,1,1,1,1,1,1,1,1,1,1,3,6]
[1,1,1,1,1,1,1,1,1,1,1,1,3,7]
```

This tell us: never mind the Langford spacings, we need to get better at generating test sequences alone, that just have each digit appear twice. Our search algorithm is too random right now.

#### A more "intelligent" domain

The domain selection above isn't very helpful.  We essentially tell Prolog to get a number from 1..7, and try to place it (and others) to meet the Langford rules. It's essentially a random search.  What if we got more clever about the domain selection to guide Prolog to the *Langford* sequence we seek?

Here, make our domain a little more restrictive at the onset. Instead of choosing a number, let's choose a number and pre-calculate the two positions in the final solution the number must appear. For example, if we choose a 1, we know it has to apppear at index $n_1$ and index $n_1+1+1$. Or, more generally, number $k$ has to appear at index $n_1$ and $n_1+k+1$.  This is what the `domain(Len,K,N1,N2)` clause does.

The `domain(Len,K,N1,N2)` clause assumes it has `Len` spots to fill in the Langford sequence. (Here "spot" also means (in array lingo) the index or position in the Langford sequence.) It finds a number `K` from `val(K)` then chooses a possible index for it (in the Langford sequence) from the `aindex` facts. We note the domain of `aindex` is always `1..Len-2`, because at minimum a `1_1` sequence might be squeezed into the last 3 positions.

So, `domain` instantiates a number, `K` and the two positions in the Langford sequence it is proposed to go (`N1` and `N2`).

As the code runs, we are maintaining our proposed Langford sequence in a single list of sub-lists.  Each sub-list has 3 elements: the number and the two positions in which it should appear in the final sequence. So if a sub-list is `[1,2,4]`, this means `1` should appear at position `2` and `4`.

The return values returned from `domain` need to be checked using the `member` sequences.

* Make sure the number `K` hasn't already been placed: `\+ member([K,_,_],L0)`
* Make sure position `N1` has not already been used by some other number: `\+ member([_,N1,_],L0)` and ` \+ member([_,_,N1],L0)`
* Make sure position `N2` has not already been used by some other number: ` \+ member([_,_,N2],L0)` and `  \+ member([_,N2,_],L0)`

Then we call `langford` recursively using `langford([[K,N1,N2]|L0],Soln)`. This prepends the latest sub-list that passes all Langford tests to
running list `L0` via the `[[K,N1,N2]|L0]` construct, and hangs onto variable `Soln` to present the final solution. 

The terminal case is when the length of the main list (or number of Langford sub-lists) reaches a length of the maximum number in the Langford number set.  (Recall each sub-lists holds the number and its **two** positions.) This terminal case will instantiate its latest solution to the 2nd variable passed into the `langford` call. 

(Note: using a list as an accumulator like this is a bit of a triumph for us; accumulators in Prolog take some getting used to.)

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

##### Turbo Prolog
Just for fun (and as a tribute to what got me started in Prolog), I thought I'd try to get this programing running on Turbo Prolog (yes, the 1980s Borland software). Here it is (running in a Dosbox on macOS):

![Turbo Prolog Langford](https://github.com/tbensky/Prolog-CLP/blob/main/src/Langford/turbo_prolog.png)

I had to make several modifications to the code, including Turbo Prolog's `domains`, `predicates`, and `clauses` sections. Also, a compound list (lists within a list)
had to be handled as per p. 202 of the Turbo Prolog 2.0 User's Guide (that we bought on ebay); see the `llist` and `list` definitions in the `domains` section, and all the `l()` and `i()` wrappers throughout (`l` means list and `i` means integer).  Lastly, there is no `is` (looks like it's `=` for assignment), the `\+` for not is now handled with `not()`, and `=<` is `<=`.

Nonetheless, you can see a solution in the "Dialog" window, noteably `[i(4),i(1),i(6)]....` indicating that 4 is to be at positions 1 and 6, and so on. Here's the code:

```prolog
% Turbo Prolog (yes, the 1980s software) implementation
% of Langford pairing for {11,22,33,44}
domains
	llist = l(list); i(integer)
	list = llist*

predicates
	length(list,integer)
	domain(integer,integer,integer,integer)
	langford(list,list)
	member(llist,llist)
	val(integer)
	aindex(integer)
	
clauses

        member(X,l([X|_])).
        member(X,l([_|T])) :- member(X,l(T)).

        length([],0).
        length([_|Xs],L) :- length(Xs,N), L = N + 1.

        langford(L,L) :- write(L), nl, length(L,4).

        langford(L0,Soln) :-
                domain(8,K,N1,N2),
        
                not(member(l([i(K),_,_]),l(L0))),
                not(member(l([_,i(N1),_]),l(L0))),
                not(member(l([_,_,i(N2)]),l(L0))),
                not(member(l([_,i(N2),_]),l(L0))),
                not(member(l([_,_,i(N1)]),l(L0))),
                langford([l([i(K),i(N1),i(N2)])|L0],Soln).


domain(Len,K,N1,N2) :-
        val(K),
        aindex(N1),
        N1max = Len - (K + 1), 
        N1 <= N1max,
        N2 = N1 + (K + 1).


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

goal
	langford([],L), write(L), nl.
```

#### Back at it
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

We were also able to run it for `n=11`, by running `time(langford([],L)).` A solution popped out after 90 minutes or `58,842,480,119 inferences, 5394.525 CPU in 5402.777 seconds (100% CPU, 10907815 Lips).`

```prolog
L = [[10, 8, 19], [11, 4, 16], [9, 7, 17], [8, 11, 20], [7, 13, 21], [6, 15, 22], [5, 12, 18], [4, 9, 14], [3, 6, 10], [2, 2, 5], [1, 1, 3]]
```

This solution is: `1,2,1,11,2,3,9,10,4,3,8,5,7,4,6,11,9,5,10,8,7,6`.

Here's the code. Note the changes are: 1) the `11` in the `length(L,11)` clause, 2) the `22` in the `domain(22,K,N1,N2)` clause 3) extending the `val` facts up to 11, 4) extending the `aindex` facts up to `20`. 

```prolog
langford(L,L) :- length(L,11).

langford(L0,Soln) :-
            domain(22,K,N1,N2),
        
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
val(8).
val(9).
val(10).
val(11).

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
aindex(13).
aindex(14).
aindex(15).
aindex(16).
aindex(17).
aindex(18).
aindex(19).
aindex(20).
```

#### Lanford 15

Just for fun, we coded up a Langford search for the set ${1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11,11,12,12,13,13,14,14,15,15}$. As of 31-Dec-2022, it has been running on a Dell server. We'll update this page if a solution pops out.


### With CLP

That's about it for trying for Langford pairs using standard Prolog. Let's get into a CLP version now. 

We post our first attempt here, but are not sure it's the best way to go. It is a start though. 

Here, we start by adapting the non-CLP Langford 7 code above using an emerging pattern: in CLP the domain picking from Prolog facts (i.e. `val` and `aindex` above) is done using constraints. In this case, we insist that all elements of our proposed Langford list `L` have values from 1..7 using the `L ins 1..7` clause. We also modified the `rule` clause to use the CLP `element` clause (and not Prolog's `nth1`).  (Note: we switched it back to use `nth1`, as it runs *much faster*??)  Also, the intermediary index `J` is computed using CLP's `#=` not `is`.

After all of the rules are applied, we label the proposed list `L`, then check that each digit appears only twice, using the sequence of `count2` calls. Here is the code.

```prolog
:- use_module(library(clpfd)).

langford(L) :-

    L = [_,_,_,_,_,_,_,_,_,_,_,_,_,_],
    L ins 1..7,

    rule(L,1),
    rule(L,2),
    rule(L,3),
    rule(L,4),
    rule(L,5),
    rule(L,6),
    rule(L,7).

rule(L,K) :-    nth1(I,L,K),
                J #= I + K + 1,
                nth1(J,L,K).
```

Sure enough, after running it we get an answer instantly, using `time((langford(L),label(L))`: 

```
?- time((langford(L),label(L))).
% 18,906 inferences, 0.001 CPU in 0.001 seconds (100% CPU, 17265753 Lips)
L = [1, 7, 1, 2, 5, 6, 2, 3, 4, 7, 5, 3, 6, 4] ;
% 2,051 inferences, 0.000 CPU in 0.000 seconds (97% CPU, 13493421 Lips)
L = [1, 7, 1, 2, 6, 4, 2, 5, 3, 7, 4, 6, 3, 5] ;
% 10,991 inferences, 0.001 CPU in 0.001 seconds (99% CPU, 14812668 Lips)
L = [1, 6, 1, 7, 2, 4, 5, 2, 6, 3, 4, 7, 5, 3] ;
% 6,331 inferences, 0.000 CPU in 0.000 seconds (99% CPU, 14487414 Lips)
L = [1, 5, 1, 6, 7, 2, 4, 5, 2, 3, 6, 4, 7, 3] ;
% 15,257 inferences, 0.001 CPU in 0.001 seconds (99% CPU, 15120912 Lips)
L = [1, 4, 1, 5, 6, 7, 4, 2, 3, 5, 2, 6, 3, 7] ;
% 7,898 inferences, 0.001 CPU in 0.001 seconds (99% CPU, 14545120 Lips)
L = [1, 4, 1, 6, 7, 3, 4, 5, 2, 3, 6, 2, 7, 5] ;
% 8,321 inferences, 0.001 CPU in 0.001 seconds (99% CPU, 11686798 Lips)
L = [1, 6, 1, 3, 5, 7, 4, 3, 6, 2, 5, 4, 2, 7] ;
% 2,377 inferences, 0.000 CPU in 0.000 seconds (95% CPU, 10471366 Lips)
L = [1, 5, 1, 7, 3, 4, 6, 5, 3, 2, 4, 7, 2, 6] 
... 
```

If we do

```prolog
?- findall(X,langford(X),X), length(X,L).
X = [[1, 7, 1, 2, 5, 6, 2, 3|...], [1, 7, 1, 2, 6, 4, 2|...], [1, 6, 1, 7, 2, 4|...], [1, 5, 1, 6, 7|...], [1, 4, 1, 5|...], [1, 4, 1|...], [1, 6|...], [1|...], [...|...]|...],
L = 52.
```

We get 52 solutions, pretty much instantly. There are 26 for n=7, but Prolog finds each solution and its reverse as well.

Curiously, we dropped testing if the numbers occur in pairs in any proposed solution.  Why? The space constraints of the Langford list itself.  The pairs of numbers must
all squeeze into the 14 slots, so if all `rule` clauses succeed, there can't be for example a solution like [1,1,1,1,...]

#### n=11: Solutions in about 0.1 seconds.

```prolog
?- time((langford(X),label(X))).
% 1,736,171 inferences, 0.100 CPU in 0.100 seconds (100% CPU, 17422689 Lips)
X = [1, 2, 1, 11, 2, 3, 9, 10, 4|...] [write]
X = [1, 2, 1, 11, 2, 3, 9, 10, 4, 3, 8, 5, 7, 4, 6, 11, 9, 5, 10, 8, 7, 6] ;
% 15,471 inferences, 0.001 CPU in 0.001 seconds (99% CPU, 12077283 Lips)
X = [1, 2, 1, 11, 2, 3, 10, 8, 4, 3, 9, 7, 5, 4, 6, 11, 8, 10, 5, 7, 9, 6] ;
% 8,590 inferences, 0.001 CPU in 0.001 seconds (99% CPU, 11718963 Lips)
X = [1, 2, 1, 11, 2, 3, 9, 10, 4, 3, 6, 7, 8, 4, 5, 11, 9, 6, 10, 7, 5, 8] ;
% 5,762 inferences, 0.001 CPU in 0.001 seconds (99% CPU, 11524000 Lips)
X = [1, 2, 1, 11, 2, 3, 10, 8, 4, 3, 7, 9, 6, 4, 5, 11, 8, 10, 7, 6, 5, 9] ;
% 67,014 inferences, 0.004 CPU in 0.004 seconds (100% CPU, 15189030 Lips)
X = [1, 2, 1, 9, 2, 3, 11, 8, 10, 3, 4, 5, 7, 9, 6, 4, 8, 5, 11, 10, 7, 6] ;
% 14,461 inferences, 0.001 CPU in 0.001 seconds (99% CPU, 12121542 Lips)
X = [1, 2, 1, 9, 2, 3, 10, 11, 7, 3, 4, 8, 5, 9, 6, 4, 7, 10, 5, 11, 8, 6] ;
% 1,279 inferences, 0.000 CPU in 0.000 seconds (94% CPU, 11842593 Lips)
X = [1, 2, 1, 9, 2, 3, 10, 8, 11, 3, 4, 7, 5, 9, 6, 4, 8, 10, 5, 7, 11, 6] ;
% 22,663 inferences, 0.002 CPU in 0.002 seconds (99% CPU, 12818439 Lips)
X = [1, 2, 1, 9, 2, 3, 10, 11, 7, 3, 4, 6, 8, 9, 5, 4, 7, 10, 6, 11, 5, 8] ;
% 31,525 inferences, 0.002 CPU in 0.002 seconds (99% CPU, 13998668 Lips)
X = [1, 2, 1, 11, 2, 3, 10, 5, 9, 3, 8, 4, 7, 5, 6, 11, 4, 10, 9, 8, 7, 6] ;
% 30,153 inferences, 0.002 CPU in 0.002 seconds (100% CPU, 13643891 Lips)
X = [1, 2, 1, 9, 2, 3, 10, 7, 11, 3, 8, 4, 5, 9, 6, 7, 4, 10, 5, 8, 11, 6] ;
% 27,279 inferences, 0.002 CPU in 0.002 seconds (99% CPU, 15316676 Lips)
X = [1, 2, 1, 11, 2, 3, 6, 9, 10, 3, 7, 4, 8, 6, 5, 11, 4, 9, 7, 10, 5, 8] ;
% 1,905 inferences, 0.000 CPU in 0.000 seconds (93% CPU, 10241935 Lips)
X = [1, 2, 1, 9, 2, 3, 10, 11, 6, 3, 7, 4, 8, 9, 5, 6, 4, 10, 7, 11, 5, 8] ;
% 3,334 inferences, 0.000 CPU in 0.000 seconds (97% CPU, 10931148 Lips)
X = [1, 2, 1, 9, 2, 3, 11, 7, 10, 3, 6, 4, 8, 9, 5, 7, 4, 6, 11, 10, 5, 8] ;
% 2,860 inferences, 0.000 CPU in 0.000 seconds (97% CPU, 10671642 Lips)
X = [1, 2, 1, 9, 2, 3, 11, 7, 8, 3, 10, 4, 6, 9, 5, 7, 4, 8, 11, 6, 5, 10] ;
% 31,746 inferences, 0.002 CPU in 0.002 seconds (99% CPU, 13531969 Lips)
X = [1, 2, 1, 11, 2, 3, 9, 5, 10, 3, 7, 8, 4, 5, 6, 11, 9, 4, 7, 10, 8, 6] ;
% 31,096 inferences, 0.002 CPU in 0.002 seconds (99% CPU, 14842959 Lips)
X = [1, 2, 1, 9, 2, 3, 11, 7, 10, 3, 5, 8, 4, 9, 6, 7, 5, 4, 11, 10, 8, 6] ;
% 1,248 inferences, 0.000 CPU in 0.000 seconds (94% CPU, 12000000 Lips)
X = [1, 2, 1, 9, 2, 3, 8, 10, 11, 3, 5, 7, 4, 9, 6, 8, 5, 4, 10, 7, 11, 6] ;
% 28,094 inferences, 0.002 CPU in 0.002 seconds (99% CPU, 12976443 Lips)
X = [1, 2, 1, 11, 2, 3, 6, 10, 7, 3, 8, 9, 4, 6, 5, 11, 7, 4, 10, 8, 5, 9] ;
% 1,087 inferences, 0.000 CPU in 0.000 seconds (89% CPU, 9134454 Lips)
X = [1, 2, 1, 11, 2, 3, 6, 8, 10, 3, 7, 9, 4, 6, 5, 11, 8, 4, 7, 10, 5, 9] ;
% 1,087 inferences, 0.000 CPU in 0.000 seconds (89% CPU, 9211864 Lips)
X = [1, 2, 1, 11, 2, 3, 6, 8, 9, 3, 10, 7, 4, 6, 5, 11, 8, 4, 9, 7, 5, 10] ;
% 3,612 inferences, 0.000 CPU in 0.000 seconds (98% CPU, 11079755 Lips)
X = [1, 2, 1, 9, 2, 3, 11, 8, 6, 3, 10, 7, 4, 9, 5, 6, 8, 4, 11, 7, 5, 10] ;
% 3,560 inferences, 0.000 CPU in 0.000 seconds (97% CPU, 10920245 Lips)
X = [1, 2, 1, 9, 2, 3, 8, 11, 7, 3, 10, 6, 4, 9, 5, 8, 7, 4, 6, 11, 5, 10] ;
% 165,220 inferences, 0.010 CPU in 0.010 seconds (100% CPU, 15964828 Lips)
X = [1, 2, 1, 11, 2, 3, 10, 5, 7, 3, 9, 6, 8, 5, 4, 11, 7, 10, 6, 4, 9, 8] ;
% 1,340 inferences, 0.000 CPU in 0.000 seconds (91% CPU, 9436620 Lips)
X = [1, 2, 1, 11, 2, 3, 9, 5, 8, 3, 10, 6, 7, 5, 4, 11, 9, 8, 6, 4, 7, 10] ;
% 5,373 inferences, 0.000 CPU in 0.000 seconds (98% CPU, 11456290 Lips)
X = [1, 2, 1, 11, 2, 3, 6, 10, 8, 3, 5, 9, 7, 6, 4, 11, 5, 8, 10, 4, 7, 9] ;
% 4,776 inferences, 0.000 CPU in 0.000 seconds (97% CPU, 14088496 Lips)
X = [1, 2, 1, 9, 2, 3, 10, 7, 11, 3, 5, 6, 8, 9, 4, 7, 5, 10, 6, 4, 11, 8] ;
% 16,188 inferences, 0.001 CPU in 0.001 seconds (99% CPU, 14837764 Lips)
X = [1, 2, 1, 11, 2, 3, 6, 10, 7, 3, 9, 5, 8, 6, 4, 11, 7, 5, 10, 4, 9, 8] ;
% 2,127 inferences, 0.000 CPU in 0.000 seconds (96% CPU, 10477833 Lips)
X = [1, 2, 1, 11, 2, 3, 6, 8, 9, 3, 10, 5, 7, 6, 4, 11, 8, 5, 9, 4, 7, 10] ;
% 3,361 inferences, 0.000 CPU in 0.000 seconds (98% CPU, 13774590 Lips)
X = [1, 2, 1, 9, 2, 3, 11, 8, 6, 3, 10, 5, 7, 9, 4, 6, 8, 5, 11, 4, 7, 10] ;
% 16,192 inferences, 0.001 CPU in 0.001 seconds (99% CPU, 14882353 Lips)
X = [1, 2, 1, 11, 2, 3, 6, 9, 7, 3, 10, 8, 5, 6, 4, 11, 7, 9, 5, 4, 8, 10] ;
% 488,246 inferences, 0.029 CPU in 0.029 seconds (100% CPU, 17061397 Lips)
X = [1, 2, 1, 11, 2, 10, 3, 4, 9, 7, 3, 8, 4, 5, 6, 11, 10, 7, 9, 5, 8, 6] ;
% 10,521 inferences, 0.001 CPU in 0.001 seconds (99% CPU, 12065367 Lips)
X = [1, 2, 1, 11, 2, 10, 3, 4, 8, 9, 3, 6, 4, 7, 5, 11, 10, 8, 6, 9, 5, 7] 
```


#### n=15: About 4 seconds for a solution.

This code for n=15, gives solutions in about 4 seconds.

```prolog
:- use_module(library(clpfd)).

langford(L) :-

    L = [
            _,_,_,_,_,_,_,_,_,_,
            _,_,_,_,_,_,_,_,_,_,
            _,_,_,_,_,_,_,_,_,_
        ],
    L ins 1..15,

    rule(L,1),
    rule(L,2),
    rule(L,3),
    rule(L,4),
    rule(L,5),
    rule(L,6),
    rule(L,7),
    rule(L,8),
    rule(L,9),
    rule(L,10),
    rule(L,11),
    rule(L,12),
    rule(L,13),
    rule(L,14),
    rule(L,15).

rule(L,K) :-    nth1(I,L,K),
                J #= I + K + 1,
                nth1(J,L,K).
```

Here are some solutions:

```prolog
?- time((langford(X),label(X))).
% 72,467,128 inferences, 4.000 CPU in 4.002 seconds (100% CPU, 18116719 Lips)
X = [1, 2, 1, 3, 2, 4, 14, 3, 15|...] [write]
X = [1, 2, 1, 3, 2, 4, 14, 3, 15, 13, 4, 5, 12, 6, 7, 10, 11, 5, 8, 9, 6, 14, 7, 13, 15, 12, 10, 8, 11, 9] ;
% 25,984 inferences, 0.002 CPU in 0.002 seconds (99% CPU, 13243629 Lips)
X = [1, 2, 1, 3, 2, 4, 14, 3, 15, 13, 4, 5, 12, 6, 7, 11, 9, 5, 10, 8, 6, 14, 7, 13, 15, 12, 9, 11, 8, 10] ;
% 84,353 inferences, 0.005 CPU in 0.005 seconds (97% CPU, 16252987 Lips)
X = [1, 2, 1, 3, 2, 4, 15, 3, 12, 14, 4, 5, 13, 6, 10, 7, 11, 5, 8, 9, 6, 12, 15, 7, 14, 10, 13, 8, 11, 9] ;
% 877 inferences, 0.000 CPU in 0.000 seconds (93% CPU, 8858586 Lips)
X = [1, 2, 1, 3, 2, 4, 14, 3, 15, 12, 4, 5, 13, 6, 10, 7, 11, 5, 8, 9, 6, 14, 12, 7, 15, 10, 13, 8, 11, 9] ;
% 28,923 inferences, 0.002 CPU in 0.002 seconds (99% CPU, 13063686 Lips)
X = [1, 2, 1, 3, 2, 4, 14, 3, 13, 15, 4, 5, 11, 6, 12, 7, 9, 5, 10, 8, 6, 14, 13, 7, 11, 15, 9, 12, 8, 10] ;
% 92,304 inferences, 0.006 CPU in 0.006 seconds (100% CPU, 14875745 Lips)
X = [1, 2, 1, 3, 2, 4, 14, 3, 13, 15, 4, 5, 10, 6, 11, 12, 7, 5, 8, 9, 6, 14, 13, 10, 7, 15, 11, 8, 12, 9] ;
% 1,868 inferences, 0.000 CPU in 0.000 seconds (93% CPU, 10263736 Lips)
X = [1, 2, 1, 3, 2, 4, 15, 3, 14, 11, 4, 5, 13, 6, 10, 12, 7, 5, 8, 9, 6, 11, 15, 14, 7, 10, 13, 8, 12, 9] 
...
```

## Solving Santa's Reindeer

[This puzzle](https://news.ycombinator.com/item?id=34224456) popped up on HN on 1/2/23:

> Vixen should be behind Rudolph, Prancer and Dasher, whilst Vixen should be in front of Dancer and Comet. Dancer should be behind Donder, Blitzen and Rudolph. Comet should be behind Cupid, Prancer and Rudolph. Donder should be behind Comet, Vixen, Dasher, Prancer and Cupid. Cupid should be in front of Comet, Blitzen, Vixen, Dancer and Rudolph. Prancer should be in front of Blitzen, Donder and Cupid. Blitzen should be behind Cupid but in front of Dancer, Vixen and Donder. Rudolph should be behind Prancer but in front of Dasher, Dancer and Donder. Finally, Dasher should be behind Prancer but in front of Blitzen, Dancer and Vixen.

The question is: "In what order should the reindeer be placed?"  For the sake of this study, the correct answer is:
```[prancer,cupid,rudolph,dasher,blitzen,vixen,comet,donder,dancer]```

or in reverse

```[dancer,donder,comet,vixen,blitzen,dasher,rudolph,cupid,prancer]```.

### Without CLP

#### First attempt

Here is a simple approach: make a list of reindeer names, then use maplist to choose numbers for them all from the `order` domain. We'll then use maplist again on
all chosen variables to enforce the ordering rules. We like this code because translating the problem into Prolog is so straightforward. Here is the code:

```prolog
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
```

The problem is that this runs and runs and doesn't seem to end. The search space is too large and the search approach is not at all guided.  As in a Langford trial above, the ordering of the reindeer variable enumeration during the search is something like this:

```
?- go(L).
[1,1,1,1,1,1,1,1,1]
[1,1,1,1,1,1,1,1,2]
[1,1,1,1,1,1,1,1,3]
[1,1,1,1,1,1,1,1,4]
[1,1,1,1,1,1,1,1,5]
[1,1,1,1,1,1,1,1,6]
```

We can see that Prolog is just counting up one by one. It's just guessing, and waiting for a case to come up that satisfies the ordering rules.  Further, all of these guesses are obviously wrong for an answer to this, since reindeer cannot share a position in line.  The code is short and direct, but the search stategy is awful.

### Second attempt: smarter domain choosing

In this next attempt, we'll dispense with the short code and work to choose the domain more wisely.

For example, since the Blizten/Cupid requirement is so simple in `Blitzen > Cupid`, why not first choose ordering values for these two, then immediately check that this requirement holds? This will immediately exclude all search paths that do not have `Blitzen > Cupid`.

Likewise, the Rudolph/Prancer condition is also simple in `Rudolph > Prancer`.  So we'll choose values for these two next, and ensure this condition is met.  We'll even do something else: let's be sure that all values chosen for all reindeer at this point (Blizten, Cupid, Rudolph, and Prancer) are not equal (again since reindeer cannot share a position in line).  This will narrow the search space even more.  In the book [Thinking as Computation](https://www.amazon.com/Thinking-Computation-First-Course-Press/dp/0262534746) this issue is discussed on p. 101 "Minimizing the guesswork: Two rules." 

So, we'll do this to start our search:

```prolog
        order(Blitzen),
        order(Cupid),

        % Blitzen should be behind Cupid 
        Blitzen > Cupid,

        order(Rudolph),
        Rudolph =\= Blitzen,
        Rudolph =\= Cupid,

        order(Prancer),
        Prancer =\= Rudolph,
        Prancer =\= Blitzen,
        Prancer =\= Cupid,

        % Rudolph should be behind Prancer
        Rudolph > Prancer,
```

After this, we'll be on a search path that has 4 of the 9 reindeer on solid logical choosing. This code returns the answer in less than 0.1 seconds:

```
?- time(go(L)).
% 1,052,741 inferences, 0.075 CPU in 0.076 seconds (99% CPU, 13974871 Lips)
L = [9, 1, 8, 5, 6, 2, 7, 4, 3] ;
```

This means Dancer=9, Prancer=1, etc.  Here is the complete code:

```prolog
go(L) :-

    L = [Dancer,Prancer,Donder,Blitzen,Vixen,Cupid,Comet,Dasher,Rudolph],

    order(Blitzen),
    order(Cupid),
    
    % Blitzen should be behind Cupid 
    Blitzen > Cupid,

    order(Rudolph),
    Rudolph =\= Blitzen,
    Rudolph =\= Cupid,
    
    order(Prancer),
    Prancer =\= Rudolph,
    Prancer =\= Blitzen,
    Prancer =\= Cupid,

    % Rudolph should be behind Prancer
    Rudolph > Prancer,

    order(Vixen),
    order(Dasher),
    % Vixen should be behind Rudolph, Prancer and Dasher,
    maplist(>(Vixen),[Rudolph,Prancer,Dasher]),

    order(Dancer),
    order(Comet),

    %  Vixen should be in front of Dancer and Comet.
    maplist(<(Vixen),[Dancer,Comet]),

    order(Donder),

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

    % but in front of Dancer, Vixen and Donder.
    maplist(<(Blitzen),[Dancer,Vixen,Donder]),    

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
```


### Classic-Prolog attempt

We still think of Prolog as a good symbol processing language, meaning we don't always want to have to go to numbers to solve a problem. Can't we just stick to the symbols as our core data (which in this case are the names of the reindeer)? We also like this approach because it allows us to translate the problem easier into Prolog.

We can put the ordering rules in using two predicates called `behind` and `front`. Both take two aguements, like `behind(X,Y)` and will mean `Y` is behind `X` in line. Similar for `front`. Here are the ordering facts then:

```prolog
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
```

To simplify our code and not have to use both predicates in our logic, we'll define `is_behind` as:

```prolog
is_behind(X,Y) :- behind(X,Y).
is_behind(X,Y) :- front(Y,X).
```

To us a classical-Prolog solution would involve some recursion, and building up a solution (likely in a list as an accumulator) by imagining Prolog going through some clauses, backtracking when it hits a false, and continuing toward the terminal `.` as long as it keeps finding `true` results.

#### Attempt #1

Here's a first attempt:

```prolog
order_ok([]).
order_ok(L) :- length(L,1).
order_ok([A,B|Tail]) :- is_behind(A,B), order_ok([B|Tail]).

go(L,L) :- 
            length(L,9),
            order_ok(L).


go(L,Soln) :-
                rd(X),
                \+ member(X,L),
                append([X],L,L1),
                go(L1,Soln).
```

We start with an `order_ok()` clause, which takes in a list and will tell you if the reindeer names are in order, as per the rules. The clauses look like:

* `order_ok([])` meaning an empty list is in the right order.
* `order_ok(L) :- length(L,1).` meaning a list with one name in it is in the right order (as there's nothing to compare it with).
* `order_ok([A,B|Tail]) :- is_behind(A,B), order_ok([B|Tail]).` Pick out the first two elements of the list, `A` and `B`. If they're in the right order, then the new list to check is `B` + the rest of the list.

If we test this on the solution:

```prolog
?- order_ok([dancer,donder,comet,vixen,blitzen,dasher,rudolph,cupid,prancer]).
true ;
```
it works.

The core solution here is run with `go(L,Soln)`, where a proposed empty list is initially passed in, with the hopes of eventually assigning it to solution list `Soln`. Here, we
pick a reindeer with `rd(X)`, and be sure it is not already in the solution with `\+ member(X,L)`.  If not, we append it to the ongoing list `L`, and
now "loop" with the list `L1` as our latest reindeer ordering. The `go(L,L)` will stop the search when a list of 9 reindeer results, and their ordering is all ok.

If we run it, we'll get a solution in about 2.6 seconds of:

```prolog
?- time(go([],L)).
% 38,700,282 inferences, 2.626 CPU in 2.632 seconds (100% CPU, 14738855 Lips)
L = [dancer, donder, comet, vixen, blitzen, dasher, rudolph, cupid, prancer] ;
```

This isn't really a great approach though, as all the search does it create a list of 9 unique reindeer names, and check to see if they're all in order. We're happy with our growing
abilities in coding in Prolog, but this is a needle-in-a-haystack search strategy, that isn't very good.

#### Attempt #2

We can improve the search by thinking a bit about what is an acceptable reindeer to choose in the `rd(X)` step. After choosing a reindeer, verifying it's not already in the solution list,
then appending it to the solution list, why don't we now check if the list is in order? 

As an example, suppose the first two reindeer chosen are not the the proper order. There is no reason to proceed with that search.  So a core like this:

```prolog
go(L,L) :-  length(L,9), order_ok(L).

go([],Soln) :- rd(X), go([X],Soln).

go(L,Soln) :-

    rd(X),
    \+ member(X,L),
    append([X],L,L1),
    order_ok(L),
    go(L1,Soln).
```

This does a *much better* job, finishing in less than 0.02 seconds, with only a fraction of the total inferences:

```
?- time(go([],L)).
% 156,145 inferences, 0.014 CPU in 0.015 seconds (96% CPU, 10903219 Lips)
L = [dancer, donder, comet, vixen, blitzen, dasher, rudolph, cupid, prancer] ;
```


Here is the complete code:

```prolog
order_ok([]).
order_ok(L) :- length(L,1).
order_ok([A,B|Tail]) :- is_behind(A,B), order_ok([B|Tail]).


% answer: [prancer,cupid,rudolph,dasher,blitzen,vixen,comet,donder,dancer]
% in reverse: [dancer,donder,comet,vixen,blitzen,dasher,rudolph,cupid,prancer]

go(L,L) :-  length(L,9), order_ok(L).

go([],Soln) :- rd(X), go([X],Soln).

go(L,Soln) :-

    rd(X),
    \+ member(X,L),
    append([X],L,L1),
    order_ok(L),
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
```  

#### Concluding with non-CLP

There are probably more non-CLP approaches to think about. We like the non-CLP approaches for the learning opportunities, because it forces us to think carefully about the search strategy. Clearly, small things can make a large difference in the run times.

Let's go off now and look into a CLP-based solutions.

        
### With CLP

With CLP, our focus really is setting up an answer structure (here a list of variables), specifying the domain for the variables, then listing all known constraints.

Our first attempt here has a good bit of practice with CLP operators and `maplist`.

```prolog
:- use_module(library(clpfd)).

go(L) :-
        L = [Vixen, Rudolph, Prancer, Dasher, Comet, Dancer, Donder, Blitzen, Cupid],
        L ins 1..9,
        
        % Vixen should be behind Rudolph, Prancer and Dasher,
        maplist(#>(Vixen),[Rudolph,Prancer,Dasher]),
        
        %  Vixen should be in front of Dancer and Comet.
        maplist(#<(Vixen),[Dancer,Comet]),

        % Dancer should be behind Donder, Blitzen and Rudolph. 
        maplist(#>(Dancer),[Donder,Blitzen,Rudolph]),
        
        % Comet should be behind Cupid, Prancer and Rudolph.
        maplist(#>(Comet),[Cupid,Prancer,Rudolph]),

        % Donder should be behind Comet, Vixen, Dasher, Prancer and Cupid. 
        maplist(#>(Donder),[Comet, Vixen, Dasher, Prancer,Cupid]),

        % Cupid should be in front of Comet, Blitzen, Vixen, Dancer and Rudolph
        maplist(#<(Cupid),[Comet, Blitzen, Vixen, Dancer,Rudolph]),

        % Prancer should be in front of Blitzen, Donder and Cupid.
        maplist(#<(Prancer),[Blitzen,Donder,Cupid]),

        % Blitzen should be behind Cupid 
        Blitzen #> Cupid,

        % but in front of Dancer, Vixen and Donder.
        maplist(#<(Blitzen),[Dancer,Vixen,Donder]),

        % Rudolph should be behind Prancer
        Rudolph #> Prancer,
        
        % but in front of Dasher, Dancer and Donder.
        maplist(#<(Rudolph),[Dasher,Dancer,Donder]),

        % Finally, Dasher should be behind Prancer
        Dasher #> Prancer,

        % but in front of Blitzen, Dancer and Vixen.
        maplist(#<(Dasher),[Blitzen,Dancer,Vixen]).
```

This runs very fast, doing much better than our best non-CLP version:

```prolog
?- time(go(L)).
% 19,206 inferences, 0.002 CPU in 0.002 seconds (89% CPU, 10392857 Lips)
L = [6, 3, 1, 4, 7, 9, 8, 5, 2].
```

Meaning, given that our initial variable list was `[Vixen, Rudolph, Prancer, Dasher, Comet, Dancer, Donder, Blitzen, Cupid]`, we get:

```prolog
        Blitzen = 5,
        Comet = 7,
        Cupid = 2,
        Dancer = 9,
        Dasher = 4,
        Donder = 8,
        Prancer = 1,
        Rudolph = 3,
        Vixen = 6
```
#### Looking closer at what CLP does

This is a good chance to stop and look more closely at what CLP is actually doing.  Suppose we comment out the line `L ins 1..9` and run the program. We'll get:

```prolog
?- go(L).
L = [_A, _B, _C, _D, _E, _F, _G, _H, _I],
_B#=<_G+ -1,
_B#=<_F+ -1,
_B#=<_D+ -1,
_C#=<_B+ -1,
_I#=<_B+ -1,
_B#=<_E+ -1,
_B#=<_F+ -1,
_B#=<_A+ -1,
_D#=<_A+ -1,
_H#=<_A+ -1,
_I#=<_A+ -1,
_A#=<_G+ -1,
_A#=<_E+ -1,
_A#=<_F+ -1,
_D#=<_A+ -1,
_C#=<_A+ -1,
_C#=<_D+ -1,
_C#=<_I+ -1,
_C#=<_G+ -1,
_C#=<_H+ -1,
_C#=<_G+ -1,
_C#=<_E+ -1,
_D#=<_F+ -1,
_D#=<_H+ -1,
_D#=<_G+ -1,
_H#=<_F+ -1,
_I#=<_F+ -1,
_H#=<_F+ -1,
_G#=<_F+ -1,
_I#=<_E+ -1,
_E#=<_G+ -1,
_I#=<_E+ -1,
_H#=<_G+ -1,
_I#=<_G+ -1,
_I#=<_H+ -1,
_I#=<_H+ -1.
```
Here we see that Prolog recasts our list of unknowns into some internal representation using `_A, _B`, etc. Without finding a single answer, it then spits out a list of constraints that it's pulled from our code. The line

```_B#=<_G+ -1,`` 

for example means that Rudolph's position has to be less than or equal to Donder's position -1. We did in fact say that Rudolph's posiiton is to be less than Donder's, so Rudolph <= Donder-1 is correct.

So CLP(FD), or constraint programming over integers works to form a series of algebraic equations to solve. With some editing (get rid of the _'s, E to EE, and I to II and =< to <=), we put this into Mathematica to solve:

![Reindeer_mathematica](https://github.com/tbensky/Prolog-CLP/blob/main/src/Santa/mma.png)

Note we also told Mathematica about the range of the variables too. Indeed A (=Vixen) comes out to be 6, etc.


## Knuth's Stanford Graphbase: five-letter words

In Knuth's Volume 4A book,  Ch 7 is titled "Combinatorial Searching," and is what got me started on the Langford pairs (above). On p. 9, he discusses a collection of five-letter words, a list of 5757 of them, that he personally compiled over 20 years for "testing many kinds of combinatorial algorithms."  

Fun! So these words will be the next domain for our Prolog-searching-CLP studies.


### Getting the words

The words can be download from [here](https://www-cs-faculty.stanford.edu/~knuth/sgb.html). (We note also that about 12,000 5 letters words are available [here](https://github.com/3b1b/videos/blob/master/_2022/wordle/data/allowed_words.txt).  We'll stick with Knuth's words for now.  A short Python script (in SGB/fix.py) reads them in and puts them into a Prolog-friendly format, like this:


```prolog
word([w,h,i,c,h],[23,8,9,3,8],'which').
word([t,h,e,r,e],[20,8,5,18,5],'there').
word([t,h,e,i,r],[20,8,5,9,18],'their').
word([a,b,o,u,t],[1,2,15,21,20],'about').
word([w,o,u,l,d],[23,15,21,12,4],'would').
word([t,h,e,s,e],[20,8,5,19,5],'these').
word([o,t,h,e,r],[15,20,8,5,18],'other').
word([w,o,r,d,s],[23,15,18,4,19],'words').
word([c,o,u,l,d],[3,15,21,12,4],'could').
word([w,r,i,t,e],[23,18,9,20,5],'write').
...
```

This is the first 10 words, put into 3 convenient Prolog formats: 1) a list of characters, each letter enumerated with a=1, b=2, etc., then the word itself as a string. You can find this list in this repo at SGB/words.pl. The line `:- include('words.pl').` is put at the stop of our code, and off we go. 

Solving Exercises 26-35 on p. 38 of Vol 4A (or at least some of them), seems like a good place to start.

### Palindromes (#29)

A palindrome is a word spelled the same forward and backward. We got a non-CLP Prolog doing this pretty quickly.

```prolog
:- include('words.pl').

go :-
        word([A,B,C,D,E],_,Word),
        A = E,
        B = D,
        write(Word).
```

For a 5-letter words, the middle letter will be the same and only checking to see if letters 1=5, and 2=4 will do it. Here are a few:

```prolog
?- go.
level
refer
radar
madam
rotor
civic
sexes
```

And, just for a little practice building a list of palidromes using `findall`.

```prolog
:- include('words.pl').

print_list([]) :- nl.
print_list([H|T]) :- write(H), nl, print_list(T).


go(L) :-
        findall(X,word([A,B,_,B,A],_,X), L),
        print_list(L).
```

#### Palindromes with DCGs

The above technique works fine, but is limited, as it only works for 5 letter words.  At this point, the use of CLP with palindromes in not clear. 

What is a next step though (for palindromes) are DCGs or "Definite Clause Grammars." DCGs in Prolog are an "easy" way to generate and test lists for certain properties,
such is if its members are the same forward and reverse (as in a palindrome). In other words, can may be able to develop a grammar that would

1. Generate valid palindrome structure if called,
1. Recognize a palindrome if presented with one.  

The big hint for this was a search for "palindromes in Prolog" that result in this code

```prolog
palindrome --> [].
palindrome --> [_].
palindrome --> [X], palindrome, [X].
```

whick works wonderfully as in:

```prolog
?- word(X,_,_), palindrome(X,[]).
X = [l, e, v, e, l] ;
X = [r, e, f, e, r] ;
X = [r, a, d, a, r] ;
X = [m, a, d, a, m] ;
X = [r, o, t, o, r] ;
X = [c, i, v, i, c] ;
X = [s, e, x, e, s] ;
X = [s, o, l, o, s] 
```

but we have no idea how this is working. 

Interpreting the code above, we see the following:

* Clause 1: An empty list is a palindrome.

* Clause 2: A single character is a palindrome (i.e. 'a' is a palindrome). We think this it the driving logic and "base case" of this formulation.

* Clause 3: A palindome is some character X + a palindrome + the same character X again.

We understand that `-->` is just syntatic-sugar in Prolog, so doing a `listing(palindrome)` gives

```prolog
?- listing(palindrome).
palindrome(A, B) :-
    A=B.
palindrome([_|A], A).
palindrome([A|B], C) :-
    palindrome(B, D),
    D=[A|C].
```

This clause work as follows:

```prolog
?- palindrome([a,b,a],[]).
true ;
false.

?- palindrome([a,b,c],[]).
false.
```

The DCGs are cool because they'll also generate structures of unbound variables that also fit as beingm palindromes:

```
?- palindrome(L,[]).
L = [] ;
L = [_] ;
L = [_A, _A] ;
L = [_A, _, _A] ;
L = [_A, _B, _B, _A] ;
L = [_A, _B, _, _B, _A] ;
L = [_A, _B, _C, _C, _B, _A] ;
L = [_A, _B, _C, _, _C, _B, _A] 
```

You can see that it 1) Will verify that a given list is a palindrome and 2) suggest a list structure that a palindrome must follow. But still, we cannot explain how this works.  So, this is a good oppotunity to learn about DCGs with Prolog.

###### Difference lists

It turns out that difference lists are integral to DCGs, so we better look at those first.  In short, a difference list always keep track of two things: 1) the front part of the list that you may be interested in,
and 2) the rest of the list that you are not, but do not want to throw out (since something downstream may be interested in it later). 

I have found difference lists and their implementation in Prolog hard to understand. They remind me a little bit of using a wildcard character at at OS prompt like

```
$ ls prog*
```

which will show all files that start with `prog` and can have whatever ending you wish. Here difference lists are sort of the the `prog` + the `*`.

Here's what I think they mean in a nutshell in Prolog:

* think of using a list in Prolog to keep track of some result your code is working on.  How do modify the list as you go?  Perhaps `append` comes to mind? Difference lists can be used in this regard.

* Suppose you make a list containing any needed information, but you always tack an uninstantiated, ununified, variable to the list as its tail.  This 
way as the list is passed around, you can add things to the list by unifying this ununified tail. But as you do so, don't forget to always add a new unbound tail, so it's always there 
for something later. 

Algebraically, it mean a list X would be represented like

```
(X+A)-A
```

where X is the part of the list you are interested in, and A is the rest of it.  Note the expression is still just equal to `X`. The `X` and the `A` are called pairs, and
since `A` may be unknown, it is sometimes called a `hole.`

Suppose we want to look at examples of how to represent the list `[a,b,c]`. This could be:

* `[a,b,c,d,e,f]-[d,e,f]` where you can see that `X` is `[a,b,c]` and `A` is `[d,e,f]`.

* `[a,b,c]-[]` where you can see that `X` is `[a,b,c]` and `A` is `[]`.


In Prolog, there are two ways of representing difference lists (from [here](https://stackoverflow.com/questions/26966055/understanding-difference-lists)) in a functor:

* `foo([a|Tail],Tail)`
or
* `foo([a|Tail]-Tail)`

although the 2nd version isn't really valid in modern Prologs, so we'll go with the first: the pair will take up two arguments of a functor. 

A difference list in a function like `foo` above is also a terminator for recursive calls, since `a` is making an assignment (or insisting on) a value of the head of a list.

As we study these lists, we're doing to work on a system that generates sequences of a's and b's. Here are the terminators for such:

```prolog
ab([a|Rest],Rest).
ab([b|Rest],Rest).
```

Where we note the pairs, `a` and `Rest` and `b` and `Rest`. Here `ab([a|Rest],Rest).` means (a+Rest)-Rest and is meant to equal `a`. Likewise, `ab([b|Rest],Rest).` means (b+Rest)-Rest, which is
just means to equal `b`.

We find it curious to see how Prolog searches for solutions here.  `?- ab(X,Y).` evaluates to:

```prolog
?- ab(X,Y).
X = [a|Y] ;
X = [b|Y].
```

We expected to see values for `X` and `Y` on both lines. If we redefine the `ab` clauses as:

```prolog
ab([a|Rest],Rest1).
ab([b|Rest],Rest1).
```

we get

```prolog
?- ab(X,Y).
X = [a|_] ;
X = [b|_].
```

This second group doesn't compile properly, as `Rest` and `Rest1` are so-called "singleton variables" and are not used in the clause at all.

Hmmm...still not clear to us what's going on.  We think Prolog is simplifying its answers. Suppose we have:

```prolog
ab([a|Rest],Rest).
ab([b|Rest],Rest).

xy([R],R).
```

If we do:

```prolog
?- xy(A,B).
A = [B].
```

We see that `A` is supposed to be the list containing `B`. Ok--makes sense.  We expected to get:

```prolog
A=[R], B=R
```

But we suppose since `R` isn't an atom, Prolog is just tying things up--eliminating uninstantiated variables in solutions; we've never seen this behavior before.

Now, when we do `ab(A,B).` we expected to see something like

```prolog
A=[a|Rest], B=Rest ;
A=[b|Rest], B=Rest
```

In both cases, it looks like Prolog is simplying things, likely because the `ab` clause contains the uninstantiated variable `Rest`. So it's thinks
"ok, `B=Rest`, so anywhere we see `Rest`, put in a `B`." (We know `Rest` is not the name of an atom though. We didn't really expect to see R-E-S-T, but
maybe just some reference to it.)

```prolog
A=[a|B] ;
A=[b|B]
```

as above.

We can get the output we more expecte by running `?- length(X,_), ab(X,Y).`, which gives

```prolog
X = [a],
Y = [] ;
X = [b],
Y = [] ;
X = [a, _A],
Y = [_A] ;
X = [b, _A],
Y = [_A] ;
X = [a, _A, _B],
Y = [_A, _B] ;
X = [b, _A, _B],
Y = [_A, _B] ;
X = [a, _A, _B, _C],
Y = [_A, _B, _C] ;
X = [b, _A, _B, _C],
Y = [_A, _B, _C] ;
X = [a, _A, _B, _C, _D],
Y = [_A, _B, _C, _D] ;
X = [b, _A, _B, _C, _D],
Y = [_A, _B, _C, _D] ;
X = [a, _A, _B, _C, _D, _E],
Y = [_A, _B, _C, _D, _E] ;
X = [b, _A, _B, _C, _D, _E],
Y = [_A, _B, _C, _D, _E] 
```

In this case, the list differencing is clear between `X` and `Y`. The `length` constraint forces Prolog to generate lists grouped by their length. So a difference
list of length 1 is the `X=[a]` and `Y=[]` then `X=[b]` and `Y=[]` combination. Of length 2, the `X = [a, _A], Y = [_A]`  and `X = [b, _A], Y = [_A]` combination.


Now let's move on to a clause that will generate output from the `ab` clauses, like this:

```prolog
gen_ab(List,Rest) :- ab(List,Rest).


gen_ab(List1,Rest) :- 
                    ab(List1,List2),
                    gen_ab(List2,Rest).
```

The first clause just echos the direct `ab` calls from above. What about the second?

This is where the hole (or rest, or tail) of `List1` (or `List2`) now gets its chance to become populated, as it's next
passed back into `gen_ab` as the first parameter.  Tracing:

* After the first `ab(List1,List2)` call, `List1` will be `[a|List2]`

* In the `gen_ab(List2,Rest).`, `List2` will also become `[a|List2]`. 

* As before, `List2` will now become `[a|Rest]`.

* So with `List1=[a|List2]` and `List2=[a|Rest]`, `List1` will become `[a|[a|Rest]]` which Prolog evaluates to `[a,a|Rest]`. 

* The `List2` to `List2` connection between the `ab()` call and the `gen_ab()` call is key here. 

** Not sure how to highlight this right now, but this idea is the key functionality here: the hole in the original list, gets filled with
a pattern that follows he rules of the terminators + its own hole.

Note (again, unknown Prolog behavior):

```prolog
?- L=[a|[a|b]].
L = [a, a|b].
```
Apparently in a single list, Prolog wants only one tail--everything else is the head.

> The key take-away from this construct is: The hole of the original list, now becomes instantiated as allowed by the terminators, generating its own new hole each time. 

Note how the hole is never filled, as it's in the position of the uninstantiated `Rest` list throughout. It's always available in the list to successfully match the "leftover junk" in a tail of a list, whose head strictly matches the rules of the terminators.

So, we see how given the difference list terminators (the `ab` clauses), `gen_ab` *generates* output like this:


```prolog
?- gen_ab(X,Y).
X = [a|Y] ;
X = [b|Y] ;
X = [a, a|Y] ;
X = [a, b|Y] ;
X = [a, a, a|Y] ;
X = [a, a, b|Y] ;
X = [a, a, a, a|Y] ;
X = [a, a, a, b|Y] ;
X = [a, a, a, a, a|Y] ;
X = [a, a, a, a, b|Y] ;
X = [a, a, a, a, a, a|Y] ;
X = [a, a, a, a, a, b|Y] ;
X = [a, a, a, a, a, a, a|Y] ;
X = [a, a, a, a, a, a, b|Y] ;
X = [a, a, a, a, a, a, a, a|Y] ;
X = [a, a, a, a, a, a, a, b|Y] ;
```

And notice that the result always has the difference list format with the `Y` in there.

The pattern seems "unfair" in terms of the a's and the b's. This is "unfair enumeration" which we learned from Triska [here](https://www.youtube.com/watch?v=CvLsVfq6cks). We can fix this by
running

```prolog
?- length(X,_), gen_ab(X,Y).
X = [a],
Y = [] ;
X = [b],
Y = [] ;
X = [a, _A],
Y = [_A] ;
X = [b, _A],
Y = [_A] ;
X = [a, a],
Y = [] ;
X = [a, b],
Y = [] ;
X = [b, a],
Y = [] ;
X = [b, b],
Y = [] ;
X = [a, _A, _B],
Y = [_A, _B] ;
X = [b, _A, _B],
Y = [_A, _B] ;
X = [a, a, _A],
Y = [_A] ;
X = [a, b, _A],
Y = [_A] ;
X = [a, a, a],
Y = [] 
```

This forces Prolog to hunt for all solutions of a given length before continuing.

##### Bi-directional

Now for some usage. The `gen_ab` clause above can be used to *test* that an input list follows the rules of the terminators, like this:

```prolog
?- gen_ab([a,1,2,3],X).
X = [1, 2, 3] ;
false.
```
Here, it sees the `a` in the head and returns the rest or hole (`X`) that consists of everything else other that isn't a single `a`. 

As another example, here it'll strip off both leading `a`'s, one-by-one, before stopping:

```prolog
?- gen_ab([a,a,1,2,3],X).
X = [a, 1, 2, 3] ;
X = [1, 2, 3] ;
false.
```

Lastly, it'll strip off any `a` or `b` from the head of a list (one by one):

```prolog
?- gen_ab([a,b,a,b,1,2,3],X).
X = [b, a, b, 1, 2, 3] ;
X = [a, b, 1, 2, 3] ;
X = [b, 1, 2, 3] ;
X = [1, 2, 3] ;
false.
```


And, as we've already seen, it can also be used to *generate* lists that follow the rules of the terminators:

```prolog
?- gen_ab(X,[1,2,3]).
X = [a, 1, 2, 3] ;
X = [b, 1, 2, 3] ;
X = [a, a, 1, 2, 3] ;
X = [a, b, 1, 2, 3] ;
X = [a, a, a, 1, 2, 3] ;
X = [a, a, b, 1, 2, 3] ;
X = [a, a, a, a, 1, 2, 3] ;
X = [a, a, a, b, 1, 2, 3] ;
X = [a, a, a, a, a, 1, 2, 3] ;
X = [a, a, a, a, b, 1, 2, 3] ;
X = [a, a, a, a, a, a, 1, 2, 3] ;
X = [a, a, a, a, a, b, 1, 2, 3] ;
```

Note the tail of `[1,2,3]` doesn't follow the terminator rules, so it just gets tacked on to the end of any list it generates that does follow the rules.

To enumerate things more fairly:

```prolog
?- length(X,_), gen_ab(X,[1,2,3]).
X = [a, 1, 2, 3] ;
X = [b, 1, 2, 3] ;
X = [a, a, 1, 2, 3] ;
X = [a, b, 1, 2, 3] ;
X = [b, a, 1, 2, 3] ;
X = [b, b, 1, 2, 3] ;
X = [a, a, a, 1, 2, 3] ;
X = [a, a, b, 1, 2, 3] ;
X = [a, b, a, 1, 2, 3] ;
X = [a, b, b, 1, 2, 3] ;
X = [b, a, a, 1, 2, 3] ;
X = [b, a, b, 1, 2, 3] ;
X = [b, b, a, 1, 2, 3] ;
X = [b, b, b, 1, 2, 3] ;
```


#### More on difference lists

We found a *really nice* reference [here](https://tmcs.math.unideb.hu/load_doc.php?p=185&t=doc), called "Difference lists in Prolog" by Attila Csenki. (This author also has two books out on Prolog.) In particular, let's look at appending two lists.  If you're studying Prolog, explaining how the traditional `append` predicate works is really difficult. (Clocksin says "Do not worry if would not have thought of this yourself."  Hint: the successor idea in Prolog help some.) 

Anyway, back at it. Append using difference lists using this simple clause `app_dl` (for append with difference lists):

```prolog
app_dl(A,B,B,A).
```

Suppose we want to append `[d,e]` to `[a,b,c]`.  We'll call this as follows:

```prolog
?- app_dl([a,b,c|X],X,[d,e],Z).
```

As you can see `A` is unified to the difference list `[a,b,c|X]`: some atoms, a, b, and c, and some unknown `X` as its tail.  Next, `B` is bound to `X`.  Next again, B is bound to `[d,e]`. But since B is bound to X, the `[d,e]` now makes it into `X`, which is that unknown tail of `A`.  Now unifying `Z` to `A` means `Z` will be `[a,b,c,d,e]` as shown here:

```prolog
?- app_dl1([a,b,c|X],X,[d,e],Z).
X = [d, e],
Z = [a, b, c, d, e].
```

Now, we suppose a weakness here is that `Z` no longer has a hole at the end of it, so `Z` is no longer a difference list, and quick appending to it is no longer possible.  If we do this, however:

```prolog
?- app_dl1([a,b,c|X],X,[d,e|Y],Z).
X = [d, e],
Z = [a, b, c, d, e|Y].
```

Then `Z` does now have an ununified variable `Y` at the end of it for future modifications, maybe something like this:

```prolog
?- app_dl1([a,b,c|X],X,[d,e|Y],Z), app_dl1(Z,Y,[f,g,h,i], L).
X = [d, e, f, g, h, i],
Y = [f, g, h, i],
Z = L, L = [a, b, c, d, e, f, g, h, i].
```

Clocksin (in Clause and Effect, p.59) has a very nice discussion on difference lists.  He also addressed is via the append operation. In his work, suppose
you have two difference lists, L1-L2 and L3-L4. This means L1 has L2 as it's hole and L3 has L4 as its hole. So if `L1=[a,b,c]`, L1-L2 would be `[a,b,c|L2] - L2`,
and if `L3=[d,e]`, L3-L4 would be `[1,2,3|L4]-L4`. He writes an append `app` like this

```prolog
app(L1,L2,L3,L4,L1,L4).
```

which works as

```prolog
?- app([a,b,c|L2],L2,[d,e|L4],L4,X,Y).
L2 = [d, e|Y],
L4 = Y,
X = [a, b, c, d, e|Y].
```

He also goes on to remark that it might be clearer to define it as

```prolog
app(A,B,B,C,A,C).
```
One can also stress the idea of a difference list, defining it as

```prolog
app(A-B,B-C,A-C).
```
which also works

```prolog
?- app([a,b,c|L2]-L2,[d,e|L4]-L4,X-Y).
L2 = [d, e|Y],
L4 = Y,
X = [a, b, c, d, e|Y].
```

The A to B, B to C, and A to C pattern is pretty standard.

#### Back to palindromes

So with the difference lists in mind, we're back to deciphering what this

```prolog
palindrome --> [].
palindrome --> [_].
palindrome --> [X], palindrome, [X].
```

does.  (Let's rename it `p`, so we'll look at

```prolog
p --> [].
p --> [_].
p --> [X], p, [X].
```

It helps to look at the Prolog this is translated into, by doing a `listings(p).`, for which we get:

```prolog
p(A, B) :- A=B.

p([_|A], A).

p([A|B], C) :-
    p(B, D),
    D=[A|C].
```

Here's what we've learned:

* the `p(A,B) :- A=B.` is the `p-->[]` line. Why? This means "an empty list is a palindrome." This is one of those fun "oh wow" logic statements, but this clause is not necessary for the definition of a palindrome. Recall this is called like `p([a,b,a],[]).` As we'll see the `[]` is passed in as an empty accumulator list, to be used later.  So, `p(A,B) :- A = B.` can only succeed if `A` is also empty (again: an empty list is a palindrome).

So, the palindrome definition could be:

```prolog
p --> [_].
p --> [X], p, [X].
```

This means all we have to interpret is

```prolog
p([_|A], A).

p([A|B], C) :-
    p(B, D),
    D=[A|C].
```


* The `p([_|A], A).` is tricky. It it a difference list, which means we're looking for a list whose head is any atom, and whose tail we can pass along to the next use in some clause.  But, all it says is a single atom  (meaning the `_`) is a palindrome. Translation: Any single digit is a palindrome, so "a" for example is palindrome (same forward and backward).  Again, this logic meaning is cool, but secretly this will serve as the "base case" for the recursive part of this definition.

* Now for the recursive part

```prolog
p([A|B], C) :-
    p(B, D),
    D=[A|C].
```

Forgetting palindromes for a moment, this clause is just a list reverser. If you look at it, the head of the incoming list `[A|C]`, which is `A` is put on to the head of list `D`, via the `[A|C]` construct. How is this a list reverser? Because the `A` is always stuck on as the *head* of list `D`. It works like this. If we call `p([a,b,a],[]).` here's what we'll get

* `[A|B]` will come in as `A=a` and `B=[b,a]`. The call to `p` will be `p([b,a],[])`, then `p([a],[])`. So, you can see how the list is getting whittled down, and will eventually hit the base base to teminate the recursion.

But, here's the thing with this approach. In the `p([_|A],A).` clause, `A` is that hole (and in this case an accumulator of the reversed list), that keeps growing with the list reversal aspect of this algorithm.

| [_\|A] | A  | p([_|A],A) fails or succeeds? |
|-------|----|--------------------------------|
| _ is [a,b,a], A=[]  | [] | fails since the head [a,b,a] is not a single atom. |




