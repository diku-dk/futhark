.. role:: ref(emphasis)

.. _futhark-c(1):

=========
futhark-c
=========

SYNOPSIS
========

futhark-c [-V] [-o outfile] infile

DESCRIPTION
===========

``futhark-c`` translates a Futhark program to sequential C code, and
then compiles that C code with gcc(1) to an executable binary program.
The standard Futhark optimisation pipeline is used, and GCC is invoked
with ``-O3``, ``-lm``, and ``-std=c99``.

The resulting program will read the arguments to the ``main`` function
from standard input and print its return value on standard output.
The arguments are read and printed in Futhark syntax, just like
futharki(1).

OPTIONS
=======

-o outfile
  Where to write the resulting binary.  By default, if the source
  program is named 'foo.fut', the binary will be named 'foo'.

-v verbose
  Enable debugging output.  If compilation fails due to a compiler
  error, the result of the last successful compiler step will be
  printed to standard error.

-h
  Print help text to standard output and exit.

-V
  Print version information on standard output and exit.

SEE ALSO
========

futharki(1), futhark-test(1)
