.. role:: ref(emphasis)

.. _futhark-run(1):

===========
futhark-run
===========

SYNOPSIS
========

futhark run [options...] <program.fut>

DESCRIPTION
===========

Execute the given program by evaluating an entry point (``main`` by
default) with arguments read from standard input, and write the
results on standard output.

``futhark run`` is very slow, and in practice only useful for testing,
teaching, and experimenting with the language.  The ``#[trace]`` and
``#[break]`` attributes are fully supported in the interpreter.
Tracing prints values to stdout in contrast to compiled code, which
prints to stderr.

OPTIONS
=======

-e NAME
  Run the given entry point instead of ``main``.

-h
  Print help text to standard output and exit.

-V
  Print version information on standard output and exit.

-w, --no-warnings
  Disable interpreter warnings.

SEE ALSO
========

:ref:`futhark-repl(1)`, :ref:`futhark-test(1)`
