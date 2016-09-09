.. role:: ref(emphasis)

.. _futhark-py(1):

==========
futhark-py
==========

SYNOPSIS
========

futhark-py [-V] [-o outfile] infile

DESCRIPTION
===========

``futhark-py`` translates a Futhark program to sequential Python code.

The resulting program will read the arguments to the ``main`` function
from standard input and print its return value on standard output.
The arguments are read and printed in Futhark syntax, just like
futharki(1).

The generated code is very slow, and likely not very useful.  It might
be more interesting to use this commands big brother,
``futhark-pyopencl``.

OPTIONS
=======

-o outfile
  Where to write the resulting binary.  By default, if the source
  program is named 'foo.fut', the binary will be named 'foo'.

-V verbose
  Enable debugging output.  If compilation fails due to a compiler
  error, the result of the last successful compiler step will be
  printed to standard error.

-h
  Print help text to standard output and exit.

-v
  Print version information on standard output and exit.

SEE ALSO
========

futhark-pyopencl(1)
