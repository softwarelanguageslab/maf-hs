Monarch: A MONadic ARCHitecture for Static Analyses through Abstract Definitional Interpreters
------------------------------------------------------------------------------------------------

Monarch is a static analysis framework for developing static analyses based on the 
abstract definitional interpreters paradigm. This artifact contains the source 
code of the framework and reflects the state of the framework at 2024-09-12.

The latest version of the framework is available at https://github.com/softwarelanguageslab/monarch.

## Setting up

### Using the Docker image

This artifact is also provided as a Docker image (in `image.tar.gz`) that 
includes all the tools with their correct versions in addition to 
a compiled executable that can be used to test the Python analysis 
provided by the framework.

The Docker image can be used as follows:
```
$ docker load < image.tar.gz
$ docker run -it monarch /bin/sh
# maf-exe python -f maf2-analysis/programs/python/Counter.py
```

The argument after `-f` can be set to any file in the set of test files.
Please note that although many Python features are already implemented, 
some features are still missing. Currently, multiple inheritance is not supported.

### Building from source

The framework can be built from source but requires a few dependencies:
* cabal: the framework was tested with cabal version 3.10
* ghc: version 9.4.8 of GHC is required for the framework to build 
successfully. 

Both of these dependencies can be easily installed using `ghcup`.

## Using the Framework

In this section we detail how the framework is structure and how an analysis 
can be constructed from its building blocks.

### Framework Structure

The framework is structured into multiple packages according to their responsibilities:
* `maf2-syntax` provides parsers and data structures (such as AST definitions) for the languages 
supported by our framework. Currently it includes support for Scheme, Python and Erlang.
* `maf2-domains` provides the basic building blocks and their combinators for constructing 
abstract domains to be used by a static analysis. 
* `maf2-analysis` provides the building blocks for expressing program semantics and 
instantiating a static analysis from this program semantics.

#### Domains Package

This package mainly consists of two modules: `Lattice` and `Domain`.
The `Lattice` module provides the basic lattice type classes (in  `Lattice.Class`) such 
as `PartialOrder` and `Joinable`. Moreover, it contains instances of these type classes
for basic Haskell data types such as `Maybe` and `Set`.

The `Domain` module refers to specific abstract domain and is split similarly to the `Lattice` 
module. It contains type classes (in `Domain.Core.*.Class`) that express abstract domain-specific operations such 
as the `NumberDomain` which specifies operations such as `plus` and `minus` and also instances for 
these type classes (for example `Domain.Core.NumberDomain.ConstantPropagation`).

For implementing combinations of domains the `HMap` structure may be used (which correspond
to the `SparseLabeledProduct` from the paper). To use them to their fullest extent both 
`Data.TypeLevel.HMap` and `Lattice.HMapLattice` must be imported. The paper contains 
more details on its usage.

#### Analysis Package

### An Analysis for a simple lambda-calculus



### Usage as a library



## Examples of the paper
