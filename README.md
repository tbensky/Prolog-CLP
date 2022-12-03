# Learning Constraint Logic Programming with Prolog

This page is a research record of my attenpt to learn Prolog programming using Constraint Logic Programming (CLP).  


## Project: Basic ideas

Here is a simple Prolog program that tries to instantiate the variablex `X`, `Y`, and `Z`.  

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
Z in 3..sup.
```

Which happily ran, acknowledging that X is 4, Y will be involved in $4+Z=Y$ and Z will be between 3 and $\infty$.

This is the basic idea of Prolog+CLP. Programs don't have to stop just becase variables are not know. It's happy to work
with just constraints on variables and continue.

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


## Project: Sudoku with and without CLP

### Without CLP

### With CLP

```Prolog

```