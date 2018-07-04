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

The generated code is very slow, likely too much to be useful.  It is
more interesting to use this command's big brother,
``futhark-pyopencl``.

OPTIONS
=======

-o outfile
  Where to write the resulting binary.  By default, if the source
  program is named 'foo.fut', the binary will be named 'foo'.

-v verbose
  Enable debugging output.  If compilation fails due to a compiler
  error, the result of the last successful compiler step will be
  printed to standard error.

--library
  Instead of compiling to an executable program, generate a Python
  module that can be ported by other Python code.  The module will
  contain a class of the same name as the Futhark source file with
  ``.fut`` removed.  Objects of the class define one method per entry
  point in the Futhark program, with matching parameters and return
  value.

--Werror
  Treat warnings as errors.

--safe
  Ignore ``unsafe`` in program and perform safety checks unconditionally.

-h
  Print help text to standard output and exit.

-V
  Print version information on standard output and exit.

SEE ALSO
========

futhark-pyopencl(1)
