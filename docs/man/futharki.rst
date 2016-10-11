.. role:: ref(emphasis)

.. _futharki(1):

==========
futharki
==========

SYNOPSIS
========

futharki [infile]

DESCRIPTION
===========

When run with no options, start an interactive futharki session.  This
is not particularly useful, but will currently let you enter an
expression, which will be parsed and the resulting AST shown.

When run with a Futhark program as the command line option, the
program is executed by evaluating the ``main`` function, and the
result printed on standard output.  The parameters to ``main`` are
read from standard input.

Futharki will run the standard Futhark optimisation pipeline before
execution, but the interpreter is still very slow.

OPTIONS
=======

-h
  Print help text to standard output and exit.

-V
  Print version information on standard output and exit.

SEE ALSO
========

futhark-c(1), futhark-test(1)
