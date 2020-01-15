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

OPTIONS ON THE GENERATED EXECUTABLE
===================================

-t timingfile
  Print the time taken to execute the program to the indicated file, an
  integral number of microseconds. The time taken to perform setup or
  teardown, including reading the input or writing the result, is not
  included in the measurement. In particular, this means that timing
  starts after all kernels have been compiled and data has been copied
  to the device buffers but before setting any kernel arguments. Timing
  stops after the kernels are done running, but before data has been
  read from the buffers or the buffers have been released.


SEE ALSO
========

:ref:`futhark-test(1)`, :ref:`futhark-cuda(1)`, :ref:`futhark-c(1)`
