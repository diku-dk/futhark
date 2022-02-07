.. role:: ref(emphasis)

.. _futhark-autotune(1):

================
futhark-autotune
================

SYNOPSIS
========

futhark autotune [options...] <program.fut>

DESCRIPTION
===========

``futhark autotune`` attemps to find optimal values for threshold
parameters given representative datasets.  This is done by repeatedly
running running the program through :ref:`futhark-bench(1)` with
different values for the threshold parameters.  When ``futhark
autotune`` finishes tuning a program ``foo.fut``, the results are
written to ``foo.fut.tuning``, which will then automatically be picked
up by subsequent uses of :ref:`futhark-bench(1)` and
:ref:`futhark-test(1)`.


OPTIONS
=======

--backend=name

  The backend used when compiling Futhark programs (without leading
  ``futhark``, e.g. just ``opencl``).

--futhark=program

  The program used to perform operations (eg. compilation).  Defaults
  to the binary running ``futhark autotune`` itself.

--pass-option=opt

  Pass an option to programs that are being run.  For example, we
  might want to run OpenCL programs on a specific device::

    futhark autotune prog.fut --backend=opencl --pass-option=-dHawaii

--runs=count

  The number of runs per data set.

-v, --verbose

  Print verbose information about what the tuner is doing.  Pass
  multiple times to increase the amount of information printed.

--skip-compilation

  Do not run the compiler, and instead assume that the program has
  already been compiled.  Use with caution.

--spec-file=FILE

  Ignore the test specification in the program file(s), and instead
  load them from this other file.  These external test specifications
  use the same syntax as normal, but *without* line comment prefixes A
  ``==`` is still expected.

--tuning=EXTENSION

  Change the extension used for tuning files (``.tuning`` by default).

--timeout=seconds

  Initial tuning timeout for each dataset in seconds. After running the intitial
  tuning run on each dataset, the timeout is based on the run time of that
  initial tuning. Defaults to 60.

  A negative timeout means to wait indefinitely.


SEE ALSO
========

:ref:`futhark-bench(1)`
