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

-g type, --generate type
  Generate a value of the indicated type, e.g. ``-g i32`` or ``-g [10]f32``.

-s int
  Set the seed used for the RNG.  Zero by default.

--T-bounds=min:max
  Set inclusive lower and upper bounds on generated values of type
  ``T``.  ``T`` is any primitive type, e.g. ``i32`` or ``f32``.  The
  bounds apply to any following uses of the ``-g`` option.  Note that
  ``i32`` does not work; use ``i32`` instead.

You can alter the output format using the following flags. To use them, add them
before data generation (--generate):

--text
  Output data in text format (must precede --generate). Default.

-b --binary
  Output data in binary Futhark format (must precede --generate).

--binary-no-header
  Output data in binary Futhark format without header (must precede --generate).

--binary-only-header
  Only output binary Futhark format header for data (must precede --generate).

EXAMPLES
========

Generate an array of floating-point numbers and an array of indices into that array::

  futhark-dataset -g [10]f32 --i32-bounds=0:9 -g [100]i32

To generate binary data, the ``--binary`` must come before the ``--generate``::

  futhark-dataset --binary --generate=[42]i32

It is possible to generate a single file containing a payload of values, and use
custom headers to make different interpretations as e.g. a 2D array. For example
we can generate a file only containing 256 ``i32``s by::

  futhark-dataset --binary-no-header --generate=[256]i32 > 256.dat

Then we can run our program with different 2D-array configurations, without
generating the array elements for each of them::

  futhark-dataset --binary-only-header --generate=[1][256]i32 | cat - 256.dat | <Futhark program>
  futhark-dataset --binary-only-header --generate=[2][128]i32 | cat - 256.dat | <Futhark program>
  futhark-dataset --binary-only-header --generate=[4][64]i32 | cat - 256.dat | <Futhark program>

SEE ALSO
========

futhark-test(1), futhark-bench(1)
