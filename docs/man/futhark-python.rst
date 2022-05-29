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
code, which depends on Numpy.

The resulting program will read the arguments to the ``main`` function
from standard input and print its return value on standard output.
The arguments are read and printed in Futhark syntax.

The generated code is very slow, likely too slow to be useful.  It is
more interesting to use this command's big brother,
:ref:`futhark-pyopencl(1)`.

OPTIONS
=======

Accepts the same options as :ref:`futhark-c(1)`.

SEE ALSO
========

:ref:`futhark-pyopencl(1)`
