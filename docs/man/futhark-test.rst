.. role:: ref(emphasis)

.. _futhark-test(1):

============
futhark-test
============

SYNOPSIS
========

futhark-test [-c | -t | -i] infiles...

DESCRIPTION
===========

This program is used to integration-test the Futhark compiler itself.
You must have futhark-c(1) and futharki(1) in your ``PATH`` when
running ``futhark-test``.  If a directory is given, all contained
files with a ``.fut`` extension are considered.

A Futhark test program is an ordinary Futhark program, with an initial
comment block describing input/output test cases and possibly other
options.  The comment block has the following overall format::

  description
  --
  cases...

The ``description`` is an arbitrary (and possibly multiline)
human-readable explanation of the test program.  It is separated from
the test cases by a line containing just ``==``.  The format of a test
case is as follows::

  [compiled] input {
    values...
  }
  output { values... } | error: regex

If ``compiled`` is present before the ``input`` keyword, this test
case will never be passed to the interpreter.  This is useful for test
cases that are annoyingly slow to interpret.  After the ``input``
block, the expected result of the test case is written as either
another block of values, or an expected run-time error, in which a
regular expression can be used to specify the exact error message
expected.  If no regular expression is given, any error message is
accepted.  If neither ``output`` nor ``error`` is given, the program
will be expected to execute succesfully, but its output will not be
validated.

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

For many usage examples, see the ``data/tests`` directory in the
Futhark source directory.  A simple example can be found in
``EXAMPLES`` below.

OPTIONS
=======

-c
  Only compile - do not run any interpreters.

-i
  Only interpret - do not run any code generators.

-t
  Only type-check - do not run programs at all.

--compiler=program

  The program used to compile Futhark programs.  This option can be
  passed multiple times, resulting in multiple compilers being used
  for each test case.  The specified program must support the same
  interface as ``futhark-c``.

--interpreter=program

  Like ``--compiler``, but for interpretation.

--typechecker=program

  Like ``--compiler``, but for when execution has been disabled with
  ``-t``.

EXAMPLES
========

The following program tests simple indexing and bounds checking::

  -- Test simple indexing of an array.
  -- ==
  -- input {
  --   [4,3,2,1]
  --   1
  -- }
  -- output {
  --   3
  -- }
  -- input {
  --   [4,3,2,1]
  --   5
  -- }
  -- error: Assertion.*failed

  fun main([]int: a:, int: i:): : int =
    a[i]

SEE ALSO
========

futhark-c(1), futharki(1)
