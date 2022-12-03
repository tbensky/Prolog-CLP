# Learning Constraint Logic Programming with Prolog

This page is a research record of my attenpt to learn Prolog programming using Constraint Logic Programming (CLP).  


## Project: Basic ideas

Here is a simple Prolog program that tries to instantiate the variablex `X`, `Y`, and `Z`.  

```prolog
go(X,Y,Z) :- 
            X is 4, 
            Y is X + Z, 
            Z > 2.
```

If this is run, we'll get:

```
?- go(X,Y,Z).
ERROR: is/2: Arguments are not sufficiently instantiated
```

It cannot run, because in the line `Y is X + Z`, `Z` has not been set, so the code fails at that line. But supposing in
some logic you are working on, `Z` will be determined, but just later on. Does the code really have to fail already?

With CLP, we can recast the code like this:

```
:- use_module(library(clpfd)).


go(X,Y,Z) :- 
            X #= 4, 
            Y #= X + Z,  
            Z #> 2.
```

Here, the traditional `is` in Prolog is replaced with `#=`, which is the CLP version of assignment. If this code is run, we get

```
?- go(X,Y,Z).
X = 4,
Y in 7..sup,
4+Z#=Y,
Z in 3..sup.
```

Which happily ran, acknowleding that X is 4, Y will be between 7 and $\infty$, $4+Z=Y$ and Z will be betweeb 3 and $\infty$.


## Project: Sudoku with and without CLP

### Without CLP

### With CLP

```Prolog

```