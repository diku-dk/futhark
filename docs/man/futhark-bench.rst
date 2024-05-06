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
Programs are compiled using the specified backend (``c`` by default),
then run a number of times for each test case, and the arithmetic mean
runtime and 95% confidence interval printed on standard output.  Refer
to :ref:`futhark-test(1)` for information on how to format test data.
A program will be ignored if it contains no data sets - it will not
even be compiled.

If compilation of a program fails, then ``futhark bench`` will abort
immediately.  If execution of a test set fails, an error message will
be printed and benchmarking will continue (and ``--json`` will write
the file), but a non-zero exit code will be returned at the end.

METHODOLOGY
===========

For each program and dataset, ``futhark bench`` first does a single
"warmup" run that is discarded.  After that it uses a two-phase
technique.

1. The *initial phase* performs ten runs (change with ``-r``), or
   perform runs for at least half a second, whichever takes longer.
   If the resulting measurements are sufficiently statistically robust
   (determined using standard deviation and autocorrelation metrics),
   the results are produced and the second phase is not entered.
   Otherwise, the results are discarded and the second phase entered.

2. The *convergence phase* keeps performing runs until a measurement
   of sufficient statistical quality is reached.

The notion of "sufficient statistical quality" is based on heuristics.
The intent is that ``futhark bench`` will in most cases do *the right
thing* by default, both when benchmarking both long-running programs
and short-running programs.  If you want complete control, disable the
convergence phase with ``--no-convergence-phase`` and set the number
of runs you want with ``-r``.

OPTIONS
=======

--backend=name

  The backend used when compiling Futhark programs (without leading
  ``futhark``, e.g. just ``opencl``).

--cache-extension=EXTENSION

  For a program ``foo.fut``, pass ``--cache-file foo.fut.EXTENSION``.
  By default, ``--cache-file`` is not passed.

--concurrency=NUM

  The number of benchmark programs to prepare concurrently.  Defaults
  to the number of cores available.  *Prepare* means to compile the
  benchmark, as well as generate any needed datasets.  In some cases,
  this generation can take too much memory, in which case lowering
  ``--concurrency`` may help.

--convergence-max-seconds=NUM

  Don't run the convergence phase for longer than this.  This does not
  mean that the measurements have converged.  Defaults to 300 seconds
  (five minutes).

--entry-point=name

  Only run entry points with this name.

--exclude-case=TAG

  Do not run test cases that contain the given tag.  Cases marked with
  "nobench", "disable", or "no_foo" (where *foo* is the backend used)
  are ignored by default.

--futhark=program

  The program used to perform operations (eg. compilation).  Defaults
  to the binary running ``futhark bench`` itself.

--ignore-files=REGEX

  Ignore files whose path match the given regular expression.

--json=file

  Write raw results in JSON format to the specified file.

--no-tuning

  Do not look for tuning files.

--no-convergence-phase

  Do not run the convergence phase.

--pass-option=opt

  Pass an option to benchmark programs that are being run.  For
  example, we might want to run OpenCL programs on a specific device::

    futhark bench prog.fut --backend=opencl --pass-option=-dHawaii

--pass-compiler-option=opt

  Pass an extra option to the compiler when compiling the programs.

--profile

  Enable profiling for the binary (by passing ``--profiling`` and
  ``--logging``) and store the recorded information in the file
  indicated by ``--json`` (which is required), along with the other
  benchmarking results.

--runner=program

  If set to a non-empty string, compiled programs are not run
  directly, but instead the indicated *program* is run with its first
  argument being the path to the compiled Futhark program.  This is
  useful for compilation targets that cannot be executed directly (as
  with :ref:`futhark-pyopencl(1)` on some platforms), or when you wish
  to run the program on a remote machine.

--runs=count

  The number of runs per data set.

--skip-compilation

  Do not run the compiler, and instead assume that each benchmark
  program has already been compiled into a server-mode executable. Use
  with caution.

--spec-file=FILE

  Ignore the test specification in the program file(s), and instead
  load them from this other file.  These external test specifications
  use the same syntax as normal, but *without* line comment prefixes.
  A ``==`` is still expected.

--suffix=STR

  Append this suffix to the program name (after stripping ``.fut``) to
  generate the name of the compiled executables. Defaults to no
  suffix.

--timeout=seconds

  If the runtime for a dataset exceeds this integral number of
  seconds, it is aborted.  Note that the time is allotted not *per
  run*, but for *all runs* for a dataset.  A twenty second limit for
  ten runs thus means each run has only two seconds (minus
  initialisation overhead).

  A negative timeout means to wait indefinitely.

-v, --verbose

  Print verbose information about what the benchmark is doing.  Pass
  multiple times to increase the amount of information printed.

--tuning=EXTENSION

  For each program being run, look for a tuning file with this
  extension, which is suffixed to the name of the program.  For
  example, given ``--tuning=tuning`` (the default), the program
  ``foo.fut`` will be passed the tuning file ``foo.fut.tuning`` if it
  exists.

EXAMPLES
========

The following program benchmarks how quickly we can sum arrays of
different sizes::

 -- How quickly can we reduce arrays?
 --
 -- ==
 -- nobench input { 0i64 }
 -- output { 0i64 }
 -- input { 100i64 }
 -- output { 4950i64 }
 -- compiled input { 10000i64 }
 -- output { 49995000i64 }
 -- compiled input { 1000000i64 }
 -- output { 499999500000i64 }

 let main(n: i64): i64 =
   reduce (+) 0 (iota n)

SEE ALSO
========

:ref:`futhark-c(1)`, :ref:`futhark-test(1)`
