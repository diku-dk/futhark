.. role:: ref(emphasis)

.. _futhark-opencl(1):

==============
futhark-opencl
==============

SYNOPSIS
========

futhark opencl [options...] infile

DESCRIPTION
===========


``futhark opencl`` translates a Futhark program to C code invoking
OpenCL kernels, and either compiles that C code with gcc(1) to an
executable binary program, or produces a ``.h`` and ``.c`` file that
can be linked with other code. The standard Futhark optimisation
pipeline is used, and GCC is invoked with ``-O3``, ``-lm``, and
``-std=c99``. The resulting program will otherwise behave exactly as
one compiled with ``futhark c``.

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

futhark-test(1), futhark-cuda(1), futhark-c(1)
