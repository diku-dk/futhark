.. role:: ref(emphasis)

.. _futhark-py(1):

==============
futhark-python
==============

SYNOPSIS
========

futhark python [options...] infile

DESCRIPTION
===========

``futhark python`` translates a Futhark program to sequential Python
code.

The resulting program will read the arguments to the ``main`` function
from standard input and print its return value on standard output.
The arguments are read and printed in Futhark syntax.

The generated code is very slow, likely too slow to be useful.  It is
more interesting to use this command's big brother,
:ref:`futhark-pyopencl(1)`.

OPTIONS
=======

-h
  Print help text to standard output and exit.

--library
  Instead of compiling to an executable program, generate a Python
  module that can be imported by other Python code.  The module will
  contain a class of the same name as the Futhark source file with
  ``.fut`` removed.  Objects of the class define one method per entry
  point in the Futhark program, with matching parameters and return
  value.

-o outfile
  Where to write the resulting binary.  By default, if the source
  program is named 'foo.fut', the binary will be named 'foo'.

--safe
  Ignore ``unsafe`` in program and perform safety checks unconditionally.

-v verbose
  Enable debugging output.  If compilation fails due to a compiler
  error, the result of the last successful compiler step will be
  printed to standard error.

-V
  Print version information on standard output and exit.

-W
  Do not print any warnings.

--Werror
  Treat warnings as errors.

SEE ALSO
========

:ref:`futhark-pyopencl(1)`
