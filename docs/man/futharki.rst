.. role:: ref(emphasis)

.. _futharki(1):

==========
futharki
==========

SYNOPSIS
========

futharki [program]

DESCRIPTION
===========

When run with no options, start an interactive Futhark session.  This
will let you interactively enter expressions and declarations which
are then immediately interpreted.  If the entered line can be either a
declaration or an expression, it is assumed to be a declaration.

Futhark source files can be loaded using the ``:load`` command.  This
will erase any interactively entered definitions.  Use the ``:help``
command to see a list of commands.  All commands are prefixed with a
colon.

When ``futharki`` is run with a Futhark program as the command line
option, the program is executed by evaluating the ``main`` function,
and the result printed on standard output.  The parameters to ``main``
are read from standard input.

``futharki`` is very slow, and in practice only useful for testing,
teaching, and experimenting with the language.  Certain special
debugging functions are available in ``futharki``:

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

futhark-c(1), futhark-test(1)
