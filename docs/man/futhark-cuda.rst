.. role:: ref(emphasis)

.. _futhark-cuda(1):

==============
futhark-cuda
==============

SYNOPSIS
========

futhark cuda [options...] infile

DESCRIPTION
===========


``futhark cuda`` translates a Futhark program to C code invoking CUDA
kernels, and either compiles that C code with gcc(1) to an executable
binary program, or produces a ``.h`` and ``.c`` file that can be
linked with other code. The standard Futhark optimisation pipeline is
used, and GCC is invoked with ``-O3``, ``-lm``, and ``-std=c99``. The
resulting program will otherwise behave exactly as one compiled with
``futhark c``.

The generated programs use the NVRTC API for run-time compilation,
which must consequently be available.

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

ENVIRONMENT
===========

If run without ``--library``, ``futhark cuda`` will invoke ``gcc(1)``
to compile the generated C program into a binary.  This only works if
``gcc`` can find the necessary CUDA libraries.  On most systems, CUDA
is installed in ``/usr/local/cuda``, which is not part of the default
``gcc`` search path.  You may need to set the following environment
variables before running ``futhark cuda``::

  LIBRARY_PATH=/usr/local/cuda/lib64
  LD_LIBRARY_PATH=/usr/local/cuda/lib64/
  CPATH=/usr/local/cuda/include

SEE ALSO
========

:ref:`futhark-opencl(1)`
