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
invoking OpenCL kernels, which depends on Numpy and PyOpenCL.  By
default, the program uses the first device of the first OpenCL
platform - this can be changed by passing ``-p`` and ``-d`` options to
the generated program (not to ``futhark pyopencl`` itself).

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

Accepts the same options as :ref:`futhark-opencl(1)`.

SEE ALSO
========

:ref:`futhark-py(1)`, :ref:`futhark-opencl(1)`
