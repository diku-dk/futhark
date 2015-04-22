.. role:: ref(emphasis)

.. _futhark-test(1):

==========
futhark-test
==========

SYNOPSIS
========

futhark-test [-c | -t | -i] infiles...

DESCRIPTION
===========

This program is used to integration-test the Futhark compiler itself.
You must have futhark-c(1) and futharki(1) in your ``PATH`` when
running ``futhark-test``.

A Futhark test program is an ordinary Futhark program, with an initial
comment block describing input/output test cases and possibly other
options.  The comment block has the following overall format::

  description
  --
  cases...

The ``description`` is an arbitrary (and possibly multiline)
human-readable explanation of the test program.  It is separated from
the test cases by a line contianing just ``--``.  The format of a test
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
accepted.

Alternatively, instead of input-output pairs, the test cases can
simply be a description of an expected compile time type error::

  error: regex

  This is used to test the type checker.

By default, both the interpreter and compiler is run on all test cases
(except those that have specified ``compiled``), although this can be
changed with command-line options to ``futhark-test``.

For many usage examples, see the ``data/tests`` directory in the
Futhark source directory.  A simple example can be found in
``EXAMPLES`` below.

OPTIONS
=======

-c
  Only compile - do not run the interpreter.

-i
  Only interpret - do not run the code generator.

-t
  Only type-check - do not run programs at all.

EXAMPLES
========

The following program tests simple indexing and bounds checking::

  // Test simple indexing of an array.
  // --
  // input {
  //   [4,3,2,1]
  //   1
  // }
  // output {
  //   3
  // }
  // input {
  //   [4,3,2,1]
  //   5
  // }
  // error: Assertion.*failed

  fun int main([int] a, int i) =
    a[i]

SEE ALSO
========

futhark-c(1), futharki(1)
