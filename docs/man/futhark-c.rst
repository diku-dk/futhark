.. role:: ref(emphasis)

.. _futhark-c(1):

=========
futhark-c
=========

SYNOPSIS
========

futhark c [options...] infile

DESCRIPTION
===========

``futhark c`` translates a Futhark program to sequential C code, and
either compiles that C code with gcc(1) to an executable binary
program, or produces a ``.h`` and ``.c`` file that can be linked with
other code..  The standard Futhark optimisation pipeline is used, and
GCC is invoked with ``-O3``, ``-lm``, and ``-std=c99``.

The resulting program will read the arguments to the entry point
(``main`` by default) from standard input and print its return value
on standard output.  The arguments are read and printed in Futhark
syntax.

OPTIONS
=======

-h
  Print help text to standard output and exit.

--library
  Generate a library instead of an executable.  Appends ``.c``/``.h``
  to the name indicated by the ``-o`` option to determine output
  file names.

-o outfile
  Where to write the result.  If the source program is named
  ``foo.fut``, this defaults to ``foo``.

--safe
  Ignore ``unsafe`` in program and perform safety checks unconditionally.

-v verbose
  Enable debugging output.  If compilation fails due to a compiler
  error, the result of the last successful compiler step will be
  printed to standard error.

-V
  Print version information on standard output and exit.

--Werror
  Treat warnings as errors.

SEE ALSO
========

:ref:`futhark-opencl(1)`, :ref:`futhark-cuda(1)`, :ref:`futhark-test(1)`
