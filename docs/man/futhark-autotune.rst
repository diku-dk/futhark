.. role:: ref(emphasis)

.. _futhark-autotune(1):

================
futhark-autotune
================

SYNOPSIS
========

futhark autotune [options...] program

DESCRIPTION
===========

``futhark-autotune`` attemps to find optimal values for threshold
parameters given representative datasets.  This is done by repeatedly
running running the program through :ref:`futhark-bench(1)` with
different values for the threshold parameters.  When
``futhark-autotune`` finishes tuning a program ``foo.fut``, the
results are written to ``foo.fut.tuning``, which will then
automatically be picked up by subsequent uses of
:ref:`futhark-bench(1)` and :ref:`futhark-test(1)`.

Currently, only the entry point named ``main`` is tuned.


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

--tuning=EXTENSION

  Change the extension used for tuning files (``.tuning`` by default).


SEE ALSO
========

:ref:`futhark-bench(1)`
