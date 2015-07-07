Quiver
======

    Copyright © 2015 Patryk Zadarnowski «pat@jantar.org».
    All rights reserved.

_Quiver_ is a powerful stream processing library for
combinatorial and monadic representation of computations
over both inductive and coinductive data streams.

It is similar to Gabriel Gonzalez's _pipes_ and
Michael Snoyman's _conduit_, but generalises both
with support for functor-based computations and
a clean support for finite (i.e., inductive) data
streams, both upstream and downstream of the computation
being defined.

The underlying data structure, `P`, is almost identical
to the `Proxy` data structure of the `Pipes` library,
except that the `Consume` and `Produce` constructors
(corresponding, respectively, to `Request` and `Response`
in the Pipes' `Proxy` data type) include an additional
argument which explicitly captures the processor's
behaviour in the event of input stream depletion
(for `Consume`) or output decoupling (for `Produce`).
This simple mechanism subsumes Conduit's need for
elaborate unconsumed-input tracking mechanisms,
and allows us to provide a mathematically-clean
framework for processing of finite data streams.

This library is currently very young, and users should
expect significant changes to the Quiver core combinators
as the underlying theory is developed and the interface
stabilises asymptotically to the future version 1.0.
