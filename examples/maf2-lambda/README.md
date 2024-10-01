# Lambda Calculus Example

This directory contains a `cabal` project which 
uses Monarch as a library for implementing 
a static analysis for a variant of the lambda calculus.

Its structure is similar to the analyses defined in the 
main Monarch packages (i.e., `maf2-analysis`). It 
is structured as folows:

* `Domain.Lambda` contains the definition of the abstract 
domain and an instantiation of this abstract domain 
using a sparse labeled product
* `Syntax.Lambda` contains the abstract syntax tree
 for the language 
* `Analysis.Lambda` contains both the semantics 
of the language as well as an instantiation of the 
analysis which computes an approximation of 
the programs memory and its results.

## Building and Running the Project

The project can be compiled and executed using `cabal`:

```
$ cabal build
$ cabal run
```

The entrypoint of this example language is configured
 to analyze the result of the following program, expressed
below in S-expression syntax:  

```scheme
((lambda (x) (+ x 1) 0)
```
