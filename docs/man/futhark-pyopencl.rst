.. role:: ref(emphasis)

.. _futhark-pyopencl(1):

================
futhark-pyopencl
================

SYNOPSIS
========

futhark pyopencl [options...] infile

DESCRIPTION
===========

``futhark pyopencl`` translates a Futhark program to Python code
invoking OpenCL kernels.  By default, the program uses the first
device of the first OpenCL platform - this can be changed by passing
``-p`` and ``-d`` options to the generated program (not to
``futhark pyopencl`` itself).

The resulting program will otherwise behave exactly as one compiled
with ``futhark py``.  While the sequential host-level code is pure
Python and just as slow as in ``futhark py``, parallel sections will
have been compiled to OpenCL, and runs just as fast as when using
``futhark opencl``.  The kernel launch overhead is significantly
higher, however, so a good rule of thumb when using
``futhark pyopencl`` is to aim for having fewer but longer-lasting
parallel sections.

The generated code requires at least PyOpenCL version 2015.2.

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

:ref:`futhark-py(1)`, :ref:`futhark-opencl(1)`
