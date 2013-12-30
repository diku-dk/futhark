L0Language
==========

The L-zero Language.  See doc/l0.tex for a language tutorial.

Installation
============

You will need GHC 7.6 to compile `l0c`, due to a bug in GHC 7.4.

Just run `cabal install` and an executable by the name of `l0c` will be
installed in your Cabal bin directory, most likely $HOME/.cabal/bin.

Otherwise, just run `cabal configure`, followed by `cabal build`, and
the executable can be found in `dist/build/l0c/l0c`.

Usage
=====

L0C requires you to manually specify every pass you want performed,
with the exception of the initial parsing and type checking.  The
general format of usage is as follows:

    l0c [options] <file>

Where `<file>` is an .l0 file (say, one of the example programs from
the `data/` directory) and the options which passes and final action
to perform.  If no options are given, the default action is to
pretty-print (`-p`) the program after type-checking.  It is also
possible to instruct `l0c` to interpret the program, by using the `-i`
option.  If the `--compile-sequential` flag is given, the program will
be compiled to C and printed on standard output.  Note, however, that
for the C code generator to work, you must manually run the
first-order transform (with `-f`) and the tuple-array transform
(`-t`).  In total, the command for compiling a program is:

    l0c -f -t --compile-sequential <program.0> > program.c

The order of passes (`-f` then `-t`) is significant.  Run `l0c` with
no arguments to see a list of possible passes.

When executing a program, L0 will start by reading (from standard
input) the arguments to the main() function.  Each argument must be
provided on a single line and be in L0 syntax.  For example, if asked
to interpret the following program, L0 will read two integers from
standard input.

    fun int main(int x, int y) = x + y

When the program finishes, the return value of main() will be printed
on standard output.

Testing
=======

Run the `data/runtests.sh` script to check how well we're doing.  Use
`data/runtests.sh -t` if you're in a hurry and only want the test
suite to do type checking.
