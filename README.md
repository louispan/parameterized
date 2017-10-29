[![Hackage](https://img.shields.io/hackage/v/parameterized.svg)](https://hackage.haskell.org/package/parameterized)
[![Build Status](https://secure.travis-ci.org/louispan/parameterized.png?branch=master)](http://travis-ci.org/louispan/parameterized)

Parameterized/indexed monoids and monads using only a single parameter type variable.

# Changelog

* 0.1.0.0
  - Initial version with parameterized Semigroup, Monoid, Applicative, Alternative, Monad
  - Added instances for OverlappingWhichReader, DistinctWhichReader, ManyReader, ManyState, and ChangingState

* 0.2.0.0
  - Renamed Pempty to PEmpty.
  - Added injective functional dependencies to PEmpty, and PEmpty.
  - TypeLevel is not exported by default
