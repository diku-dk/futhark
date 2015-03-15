Futhark
==========

The Futhark Language.  See doc/futhark.tex for a language tutorial.

Installation
============

You will need GHC 7.8 to compile Futhark.

Just run `cabal install` and an executable by the name of `futhark` will be
installed in your Cabal bin directory, most likely $HOME/.cabal/bin.

Otherwise, just run `cabal configure`, followed by `cabal build`, 
and the executable can be found in `dist/build/futhark/futhark`.

Cosmin's Experience with Instalation:
=====================================
It is not so easy as the text above claims. 
Many thanks to Troels and Rasmus. I had to start clean:
    `$ rm -r ~/.ghc; rm -r ~/.cabal`
Then try to install the latest cabal. However this 
had a dependency on "zlib1g-dev", hence:
    `$ sudo apt-get install zlib1g-dev`
    `$ cabal install cabal-install`
Then you have to install "happy" and "alex", and then
finally `make install` to build futhark, but this is 
NOT straightforward as some of the dependecies (packages)
need to be forced reinstalled. Hence first:
    `$ cabal install random text stm --reinstall --force-reinstall`
    `$ cabal install transformers exception-transformers --reinstall --force-reinstall`
and finally:
    `$ cabal install alex happy`
    `$ cabal install`

Usage
=====

Futhark requires you to manually specify every pass you want
performed, with the exception of the initial parsing and type
checking.  The general format of usage is as follows:

    futhark [options] <file>

Where `<file>` is an .l0 file (say, one of the example programs from
the `data/` directory) and the options which passes and final action
to perform.  If no options are given, the default action is to
pretty-print (`-p`) the program after type-checking.  It is also
possible to instruct `futhark` to interpret the program, by using the
`-i` option.  If the `--compile-sequential` flag is given, the program
will be compiled to C and printed on standard output.  Note, however,
that for the C code generator to work, you must manually run the
first-order transform (with `-f`), although this pass is automatically
run when using the "standard" pipeline (`-s`).  In total, the command
for compiling a program is:

    futhark -s --compile-sequential program.l0 > program.c

Run `futhark` with no arguments to see a list of possible passes and
other options.

When executing a program, Futhark will start by reading (from standard
input) the arguments to the main() function.  Each argument must be
provided on a single line and be in Futhark syntax.  For example, if
asked to interpret the following program, Futhark will read two
integers from standard input.

    fun int main(int x, int y) = x + y

When the program finishes, the return value of main() will be printed
on standard output.

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
