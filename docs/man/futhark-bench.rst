.. role:: ref(emphasis)

.. _futhark-bench(1):

=============
futhark-bench
=============

SYNOPSIS
========

futhark bench [options...] programs...

DESCRIPTION
===========

This tool is the recommended way to benchmark Futhark programs.
Programs are compiled using the specified backend (``futhark c`` by
default), then run a number of times for each test case, and the
average runtime printed on standard output.  Refer to
:ref:`futhark-test(1)` for information on how to format test data.  A
program will be ignored if it contains no data sets - it will not even
be compiled.

If compilation of a program fails, then ``futhark bench`` will abort
immediately.  If execution of a test set fails, an error message will
be printed and benchmarking will continue (and ``--json`` will write
the file), but a non-zero exit code will be returned at the end.

OPTIONS
=======

--backend=name

  The backend used when compiling Futhark programs (without leading
  ``futhark``, e.g. just ``opencl``).

--entry-point=name

  Only run entry points with this name.

--exclude-case=TAG

  Do not run test cases that contain the given tag.  Cases marked with
  "nobench" or "disable" are ignored by default.

--futhark=program

  The program used to perform operations (eg. compilation).  Defaults
  to the binary running ``futhark bench`` itself.

--ignore-files=REGEX

  Ignore files whose path match the given regular expression.

--json=file

  Write raw results in JSON format to the specified file.

--no-tuning

  Do not look for tuning files.

--pass-option=opt

  Pass an option to benchmark programs that are being run.  For
  example, we might want to run OpenCL programs on a specific device::

    futhark bench prog.fut --backend=opencl --pass-option=-dHawaii

--runner=program

  If set to a non-empty string, compiled programs are not run
  directly, but instead the indicated *program* is run with its first
  argument being the path to the compiled Futhark program.  This is
  useful for compilation targets that cannot be executed directly (as
  with :ref:`futhark-csharp(1)`), or when you wish to run the program
  on a remote machine.

--runs=count

  The number of runs per data set.

--skip-compilation

  Do not run the compiler, and instead assume that each benchmark
  program has already been compiled.  Use with caution.

--timeout=seconds

  If the runtime for a dataset exceeds this integral number of
  seconds, it is aborted.  Note that the time is allotted not *per
  run*, but for *all runs* for a dataset.  A twenty second limit for
  ten runs thus means each run has only two seconds (minus
  initialisation overhead).

  A negative timeout means to wait indefinitely.

--tuning=EXTENSION

  For each program being run, look for a tuning file with this
  extension, which is suffixed to the name of the program.  For
  example, given ``--tuning=tuning`` (the default), the program
  ``foo.fut`` will be passed the tuning file ``foo.fut.tuning`` if it
  exists.

WHAT FUTHARK BENCH MEASURES
===========================

``futhark bench`` measures the time it takes to run the given Futhark
program. It starts measuring after all kernels have been compiled and
data has been copied to the device buffers but before setting any kernel
arguments. It stops measuring after the kernels are done running and
data has been read off the buffer but before releasing the buffers.

EXAMPLES
========

The following program benchmarks how quickly we can sum arrays of
different sizes::

  -- How quickly can we reduce arrays?
  --
  -- ==
  -- nobench input { 0 }
  -- output { 0 }
  -- input { 100 }
  -- output { 4950 }
  -- compiled input { 100000 }
  -- output { 704982704 }
  -- compiled input { 100000000 }
  -- output { 887459712 }

  let main(n: i32): i32 =
    reduce (+) 0 (iota n)

SEE ALSO
========

:ref:`futhark-c(1)`, :ref:`futhark-test(1)`
