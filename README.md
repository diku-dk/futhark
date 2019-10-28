<img src="assets/logo.svg" height="50px"/> The Futhark Programming Language
==========

[![Join the chat at https://gitter.im/futhark-lang/Lobby](https://badges.gitter.im/futhark-lang/Lobby.svg)](https://gitter.im/futhark-lang/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)[![Build Status](https://travis-ci.org/diku-dk/futhark.svg?branch=master)](https://travis-ci.org/diku-dk/futhark)[![Build Status](https://dev.azure.com/futhark/futhark/_apis/build/status/diku-dk.futhark?branchName=master)](https://dev.azure.com/futhark/futhark/_build/latest?definitionId=1&branchName=master)

Futhark is a purely functional data-parallel programming language.
Its optimising compiler is able to compile it to typically very
performant GPU code.  The language and compiler is developed at
[DIKU](http://diku.dk) at the University of Copenhagen, originally as
part of the [HIPERFIT centre](http://hiperfit.dk).  The language and
compiler is quite stable, and suitable for practical programming.

For more information, see [the website](http://futhark-lang.org).

There also exists a book, [Parallel Programming in Futhark](https://futhark-book.readthedocs.io/en/latest/), that serves as an extensive introduction and guide.

Also see the [compiler and language
documentation](http://futhark.readthedocs.io) and the [basis library
documentation](https://futhark-lang.org/docs).

[Installation instructions here.](http://futhark.readthedocs.io/en/latest/installation.html)

[![Packaging status](https://repology.org/badge/vertical-allrepos/futhark.svg)](https://repology.org/project/futhark/versions)

Usage
=====

To compile a Futhark program to sequential C:

    futhark c prog.fut -o prog

Or maybe OpenCL:

    futhark opencl prog.fut -o prog

And then run it:

    ./prog < prog.input

To interpret a Futhark program:

    futhark run prog.fut < prog.input

Hacking
=======

We try to make use of Github issues for organising our work.  Issues
tagged with
[good first issue](https://github.com/diku-dk/futhark/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+issue%22)
do not require deep knowledge of the code base.

Testing
=======

Run `futhark test tests` to check how well we're doing.  Use `futhark
test -t` if you're in a hurry and only want to check that all the
tests type-check.
