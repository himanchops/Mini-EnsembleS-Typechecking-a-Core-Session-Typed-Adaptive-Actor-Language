# Mini-EnsembleS: Typechecking a Core Session-Typed Adaptive Actor Language

## Overview
Dynamic self-adaptation involves discovering, replacing, and interacting with
components at runtime. Ensemble [1] is an actor-based concurrent programming
language with native support for adaptation.

In order to rule out issues such as deadlocks, we have recently added multiparty
session types [3,4] to Ensemble, which allow us to statically check that each
actor conforms to a predefined communication protocol. We call this language
EnsembleS [2].

Our current implementation builds directly on top of Ensemble, which introduces
some implementation artifacts and is quite a bit more complicated than the core
language design.

The goal of this project is to implement a typechecker for a much smaller and
cleaner core language, which we can then use as a basis for further extensions
and experiments.

## Objectives

  * Familiarise yourself with session types.
  * Design a concrete syntax and implement a parser into an abstract syntax
      tree.
  * Implement a typechecker.
  * (Extension) Implement a runtime system for the language, allowing
      Mini-EnsembleS programs to be run
  * (Stretch extension) Investigate extensions to the language, such as more
      flexible exception handling, or the ability for an actor to take part in
      multiple sessions

## References

[1] Paul Harvey, Joseph S. Sventek.
    Adaptable Actors: Just What The World Needs. PLOS@SOSP 2017.

[2] Paul Harvey, Simon Fowler, Ornela Dardha, Simon J. Gay.
    Multiparty Session Types for Safe Runtime Adaptation in an Actor Language. ECOOP 2021.

[3] Kohei Honda, Nobuko Yoshida, Marco Carbone.
    Multiparty asynchronous session types. POPL 2008.

[4] Nobuko Yoshida, Lorenzo Gheri.
    A Very Gentle Introduction to Multiparty Session Types. ICDCIT 2020.

