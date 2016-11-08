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
will let you interactively enter expressions which are then
immediately interpreted.  You cannot enter function definitions and
the like directly, but you can load Futhark source files using the
``:load`` command.  Use the ``:help`` command to see a list of
commands.  All commands are prefixed with a colon.

expression, which will be parsed and the resulting AST shown.

When ``futharki`` is run with a Futhark program as the command line
option, the program is executed by evaluating the ``main`` function,
and the result printed on standard output.  The parameters to ``main``
are read from standard input.

Futharki will run the standard Futhark optimisation pipeline before
execution, but the interpreter is still very slow.

OPTIONS
=======

-h
  Print help text to standard output and exit.

-V
  Print version information on standard output and exit.

BUGS
====

Input editing is not yet implemented; we recommend running
``futharki`` via ``rlwrap(1)``.

SEE ALSO
========

futhark-c(1), futhark-test(1)
