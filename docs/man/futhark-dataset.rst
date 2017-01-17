.. role:: ref(emphasis)

.. _futhark-dataset(1):

===============
futhark-dataset
===============

SYNOPSIS
========

futhark-dataset options...

DESCRIPTION
===========

Generate random values in Futhark syntax, which can be useful when
generating input datasets for program testing.  All Futhark primitive
types are supported.  Tuples are not supported.  Arrays of specific
(non-random) sizes can be generated.  You can specify maximum and
minimum bounds for values, as well as the random seed used when
generating the numbers.  The generated values are written to standard
output.

OPTIONS
=======

--g type
  Generate a value of the indicated type, e.g. ``-g i32`` or ``-g [10]f32``.

-s int
  Set the seed used for the RNG.  Zero by default.

--T-bounds=min:max
  Set inclusive lower and upper bounds on generated values of type
  ``T``.  ``T`` is any primitive type, e.g. ``i32`` or ``f32``.  The
  bounds apply to any following uses of the ``-g`` option.  Note that
  ``i32`` does not work; use ``i32`` instead.

EXAMPLES
========

Generate an array of floating-point numbers and an array of indices into that array::

  futhark-dataset -g [10]f32 --i32-bounds=0:9 -g [100]i32

SEE ALSO
========

futhark-test(1), futhark-bench(1)
