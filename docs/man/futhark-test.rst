.. role:: ref(emphasis)

.. _futhark-test(1):

============
futhark-test
============

SYNOPSIS
========

futhark-test [-c | -C | -t | -i] infiles...

DESCRIPTION
===========

This program is used to integration-test the Futhark compiler itself.
You must have futhark-c(1) and futharki(1) in your ``PATH`` when
running ``futhark-test``.  If a directory is given, all contained
files with a ``.fut`` extension are considered.

A Futhark test program is an ordinary Futhark program, with at least
one test block describing input/output test cases and possibly other
options.  A test block consists of commented-out text with the
following overall format::

  description
  ==
  cases...

The ``description`` is an arbitrary (and possibly multiline)
human-readable explanation of the test program.  It is separated from
the test cases by a line containing just ``==``.  Any comment starting
at the beginning of the line, and containing a line consisting of just
``==``, will be considered a test block.  The format of a test case is
as follows::

  [tags { tags... }]
  [entry: name]
  [compiled|nobench] input {
    values...
  }
  output { values... } | error: regex

If ``compiled`` is present before the ``input`` keyword, this test
case will never be passed to the interpreter.  This is useful for test
cases that are annoyingly slow to interpret.  The ``nobench`` keyword
is for data sets that are too small to be worth benchmarking, and only
has meaning to futhark-bench(1).

After the ``input`` block, the expected result of the test case is
written as either another block of values, or an expected run-time
error, in which a regular expression can be used to specify the exact
error message expected.  If no regular expression is given, any error
message is accepted.  If neither ``output`` nor ``error`` is given,
the program will be expected to execute succesfully, but its output
will not be validated.

Alternatively, instead of input-output pairs, the test cases can
simply be a description of an expected compile time type error::

  error: regex

This is used to test the type checker.

By default, both the interpreter and compiler is run on all test cases
(except those that have specified ``compiled``), although this can be
changed with command-line options to ``futhark-test``.

Tuple syntax is not supported when specifying input and output values.
Instead, you can write an N-tuple as its constituent N values.  Beware
of syntax errors in the values - the errors reported by
``futhark-test`` are very poor.

An optional tags specification is permitted in the first test block.
This section can contain arbitrary tags that classify the benchmark::

  tags { names... }

Tag are sequences of alphanumeric characters, with each tag seperated
by whitespace.  Any program with the ``disable`` tag is ignored by
``futhark-test``.

Another optional directive is ``entry``, which specifies the entry
point to be used for testing.  This is useful for writing programs
that test libraries with multiple entry points.  The ``entry``
directive affects subsequent input-output pairs in the same comment
block, and may only be present immediately preceding these
input-output pairs.  If no ``entry`` is given, the default of ``main``
is assumed.  See below for an example.

For many usage examples, see the ``tests`` directory in the
Futhark source directory.  A simple example can be found in
``EXAMPLES`` below.

OPTIONS
=======

--notty
  Print each result on a line by itself, without line buffering.

--exclude=tag
  Ignore benchmarks with the specified tag.

-c
  Only run compiled code - do not run any interpreters.

-i
  Only interpret - do not run any compilers.

-C
  Compile the programs, but do not run them.

-t
  Type-check the programs, but do not run them.

--compiler=program
  The program used to compile Futhark programs.  This option can be
  passed multiple times, with the last taking effect.  The specified
  program must support the same interface as ``futhark-c``.

--interpreter=program

  Like ``--compiler``, but for interpretation.

--typechecker=program

  Like ``--compiler``, but for when execution has been disabled with
  ``-t``.

--runner=program

  If this is set to the non-empty string, compiled programs are not
  run directly, but instead the indicated program is run, with the
  path to the compiled Futhark program passed as the first
  command-line argument.  This is useful for compilation targets that
  cannot be executed directly (like `futhark-cs(1)`), or when you wish
  to run the program on a remote machine.

--pass-option=opt

  Pass an option to benchmark programs that are being run.  For
  example, we might want to run OpenCL programs on a specific device::

    futhark-bench prog.fut --compiler=futhark-opencl --pass-option=-dHawaii

EXAMPLES
========

The following program tests simple indexing and bounds checking::

  -- Test simple indexing of an array.
  -- ==
  -- tags { firsttag secondtag }
  -- input { [4,3,2,1] 1 }
  -- output { 3 }
  -- input { [4,3,2,1] 5 }
  -- error: Assertion.*failed

  let main (a: []i32) (i: i32): i32 =
    a[i]

The following program contains two entry points, both of which are
tested::

  let add(x: i32, y: i32): i32 = x + y

  -- Test the add1 function.
  -- ==
  -- entry: add1
  -- input { 1 } output { 2 }

  entry add1 (x: i32): i32 = add x 1

  -- Test the sub1 function.
  -- ==
  -- entry: sub1
  -- input { 1 } output { 0 }

  entry sub1 (x: i32): i32 = add x (-1)

SEE ALSO
========

futhark-c(1), futharki(1)
