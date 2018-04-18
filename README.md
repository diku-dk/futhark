<img src="assets/logo.svg" height="50px"/> The Futhark Programming Language
==========

[![Join the chat at https://gitter.im/futhark-lang/Lobby](https://badges.gitter.im/futhark-lang/Lobby.svg)](https://gitter.im/futhark-lang/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)[![Build Status](https://travis-ci.org/diku-dk/futhark.svg?branch=master)](https://travis-ci.org/diku-dk/futhark) [![Project Stats](https://www.openhub.net/p/futharkcompiler/widgets/project_thin_badge.gif)](https://www.openhub.net/p/futharkcompiler)

Futhark is a purely functional data-parallel programming language.
Its optimising compiler is able to compile it to typically very
performant GPU code.  The language and compiler is developed at
[DIKU](http://diku.dk) at the University of Copenhagen, originally as
part of the [HIPERFIT](http://hiperfit.dk).  Although still under
heavy development, Futhark is already useful for practical
high-performance programming.

For more information, see [the website](http://futhark-lang.org).

Also see the [compiler and language
documentation](http://futhark.readthedocs.io) and the [basis library
documentation](https://futhark-lang.org/docs).

[Installation instructions here.](http://futhark.readthedocs.io/en/latest/installation.html)

Usage
=====

To compile a Futhark program to sequential C:

    futhark-c prog.fut -o prog

Or maybe OpenCL:

    futhark-opencl prog.fut -o prog

And then run it:

    ./prog < prog.input

To interpret a Futhark program:

    futharki prog.fut < prog.input

Hacking
=======

We try to make use of Github issues for organising our work.  Issues
tagged with
[newbie-friendly](https://github.com/diku-dk/futhark/issues?q=is%3Aissue+is%3Aopen+label%3Anewbie-friendly)
do not require deep knowledge of the code base.

Testing
=======

Run the `futhark-test tests` to check how well we're doing.  Use
`futhark-test -t` if you're in a hurry and only want to check that all
the tests type-check.
