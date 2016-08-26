.. role:: ref(emphasis)

.. _futhark-bench(1):

=============
futhark-bench
=============

SYNOPSIS
========

futhark-bench [--runs=count | --compiler=program | --json | --no-validate] programs...

DESCRIPTION
===========

This program is used to benchmark Futhark programs.  In addition to
the notation used by ``futhark-test(1)``, this program also supports
the dataset keyword ``nobench``.  This is used to indicate datasets
that are worthwhile for testing, but too small to be worth
benchmarking.

Programs are compiled using the specified compiler (``futhark-c`` by
default), then run a number of times for each data set, and the
average runtime printed on standard output.

OPTIONS
=======

--runs=count

  The number of runs per data set.

--compiler=program

  The program used to compile Futhark programs.  This option can be
  passed multiple times, resulting in multiple compilers being used
  for each test case.  The specified program must support the same
  interface as ``futhark-c``.

--json=file

  Write raw results in JSON format to the specified file.

EXAMPLES
========

The following program benchmarks how quickly we can sum arrays of
different sizes::

  -- How quickly can we reduce arrays?
  --
  -- ==
  -- input { 0 }
  -- output { 0 }
  -- input { 100 }
  -- output { 4950 }
  -- compiled input { 100000 }
  -- output { 704982704 }
  -- compiled input { 100000000 }
  -- output { 887459712 }

  fun main(n: int): int =
    reduce(+, 0, iota(n))

SEE ALSO
========

futhark-c(1), futhark-test(1)
