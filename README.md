Futhark
==========

The Futhark Language.  The [incomplete documentation is
here](http://futhark.readthedocs.org).

Installation
============

You will need GHC 7.8 to compile Futhark.

Just run `cabal install` and executables by the names of `futhark`,
``futhark-c` and `futharki` will be installed in your Cabal bin
directory, most likely $HOME/.cabal/bin.

Otherwise, just run `cabal configure`, followed by `cabal build`, 
and the executable can be found in `dist/build/futhark/futhark`.

Make sure that you have a recent Cabal to go with your GHC.  A common
mistake is to only upgrade GHC, but use an old Cabal (i.e. older than
1.20; check with cabal --version).  If you compile some of the
dependencies with an old Cabal by mistake, and then fail later in the
process, you will have to delete your `~/.ghc` directory to remove the
broken libraries.

Usage
=====

To compile a Futhark program to sequential C:

    futhark prog.fut -o prog

And then run it:

    ./prog < prog.input

To interpret a Futhark program:

    futharki prog.fut < prog.input

Hacking
=======

We try to make use of Github issues, which are further [organised on
waffle.io](https://waffle.io/HIPERFIT/futhark).

For every commit, Futhark is automatically built and tested [on
Travis](https://travis-ci.org/HIPERFIT/futhark).

[![Stories in Ready](https://badge.waffle.io/hiperfit/futhark.png?label=ready&title=Ready)](https://waffle.io/hiperfit/futhark)

Testing
=======

Run the `data/runtests.sh` script to check how well we're doing.  Use
`data/runtests.sh -t` if you're in a hurry and only want the test
suite to do type checking.
