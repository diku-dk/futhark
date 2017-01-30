Futhark
==========

Futhark is a purely functional data-parallel programming language.
Its optimising compiler is able to compile it to typically very
performant GPU code.  The language and compiler is developed as part
of the [HIPERFIT](http://hiperfit.dk) project at
[DIKU](http://diku.dk).  The project is still research-quality, and
the focus has mostly been on ensuring high runtime-performance, so the
source language still lacks many niceties for manual programming - it
is fairly usable as a target for high-level code generation, however.

For more information, see [the website](http://futhark-lang.org).

The [incomplete documentation is
here](http://futhark.readthedocs.io).

[Installation instructions here.](http://futhark.readthedocs.io/en/latest/installation.html)

Statistics
==========

[![Issue Stats](http://issuestats.com/github/HIPERFIT/futhark/badge/pr)](http://issuestats.com/github/HIPERFIT/futhark) [![Issue Stats](http://issuestats.com/github/HIPERFIT/futhark/badge/issue)](http://issuestats.com/github/HIPERFIT/futhark) [![Build Status](https://travis-ci.org/HIPERFIT/futhark.svg?branch=master)](https://travis-ci.org/HIPERFIT/futhark) [![Project Stats](https://www.openhub.net/p/futharkcompiler/widgets/project_thin_badge.gif)](https://www.openhub.net/p/futharkcompiler)

Installation
============

Please see our [installation instructions](https://futhark.readthedocs.io/en/latest/installation.html).

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
[newbie-friendly](https://github.com/HIPERFIT/futhark/issues?q=is%3Aissue+is%3Aopen+label%3Anewbie-friendly)
do not require deep knowledge of the code base.

Testing
=======

Run the `futhark-test tests` to check how well we're doing.  Use
`futhark-test -t` if you're in a hurry and only want to check that all
the tests compile.
