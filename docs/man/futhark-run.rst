.. role:: ref(emphasis)

.. _futhark-run(1):

===========
futhark-run
===========

SYNOPSIS
========

futhark run [program]

DESCRIPTION
===========

Execute the given program by evaluating the ``main`` function with
arguments read from standard input, and write the results on standard
output.

``futhark-run`` is very slow, and in practice only useful for testing,
teaching, and experimenting with the language.  Certain special
debugging functions are available in ``futhark-run``:

``trace 'a : a -> a``
  Semantically identity, but prints the value on standard output.

``break 'a : a -> a``
  Semantically identity, but interrupts execution at the calling
  point, such that the environment can be inspected.  Continue
  execution by entering an empty input line.  Breakpoints are only
  respected when starting a program from the prompt, not when
  passing a program on the command line.

OPTIONS
=======

-e NAME
  Run the given entry point instead of ``main``.

-h
  Print help text to standard output and exit.

-V
  Print version information on standard output and exit.

SEE ALSO
========

futhark-repl(1), futhark-test(1)
