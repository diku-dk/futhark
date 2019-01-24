.. role:: ref(emphasis)

.. _futhark-bench(1):

=============
futhark-bench
=============

SYNOPSIS
========

futhark bench [--runs=count | --compiler=program | --json | --no-validate] programs...

DESCRIPTION
===========

This tool is the recommended way to benchmark Futhark programs.
Programs are compiled using the specified backend (``futhark c`` by
default), then run a number of times for each test case, and the
average runtime printed on standard output.  Test data is indicated as
with ``futhark test``.  A program will be ignored if it contains no
data sets - it will not even be compiled.

If compilation or running fails, an error message will be printed and
benchmarking will continue (and ``--json`` will write the file), but a
non-zero exit code will be returned at the end.

OPTIONS
=======

--runs=count

  The number of runs per data set.

--backend=name

  The backend used when compiling Futhark programs (without leading
  ``futhark``, e.g. just ``opencl``).

--futhark=program

  The binary used to perform operations.  Defaults to ``futhark``.

--runner=program

  If this is set to the non-empty string, compiled programs are not
  run directly, but instead the indicated program is run, with the
  path to the compiled Futhark program passed as the first
  command-line argument.  This is useful for compilation targets that
  cannot be executed directly (as with `futhark-cs(1)`), or when you
  wish to run the program on a remote machine.

--json=file

  Write raw results in JSON format to the specified file.

--pass-option=opt

  Pass an option to benchmark programs that are being run.  For
  example, we might want to run OpenCL programs on a specific device::

    futhark bench prog.fut --codegen=opencl --pass-option=-dHawaii

--timeout=seconds

  If the runtime for a dataset exceeds this integral number of
  seconds, it is aborted.  Note that the time is allotted not *per
  run*, but for *all runs* for a dataset.  A twenty second limit for
  ten runs thus means each run has only two seconds (minus
  initialisation overhead).

  A negative timeout means to wait indefinitely.

--skip-compilation

  Do not run the compiler, and instead assume that each benchmark
  program has already been compiled.  Use with caution.

--exclude-case=TAG

  Do not run test cases that contain the given tag.  Cases marked with
  "nobench" or "disable" are ignored by default.

--ignore-files=REGEX

  Ignore files whose path match the given regular expression.

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

futhark-c(1), futhark-test(1)
