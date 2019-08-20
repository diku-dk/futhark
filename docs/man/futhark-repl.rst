.. role:: ref(emphasis)

.. _futhark-repl(1):

============
futhark-repl
============

SYNOPSIS
========

futhark repl

DESCRIPTION
===========

Start an interactive Futhark session.  This will let you interactively
enter expressions and declarations which are then immediately
interpreted.  If the entered line can be either a declaration or an
expression, it is assumed to be a declaration.

Futhark source files can be loaded using the ``:load`` command.  This
will erase any interactively entered definitions.  Use the ``:help``
command to see a list of commands.  All commands are prefixed with a
colon.

``futhark-repl`` uses the Futhark interpreter, which grants access to
certain special functions.  See :ref:`futhark-run(1)` for a description.

OPTIONS
=======

-h
  Print help text to standard output and exit.

-V
  Print version information on standard output and exit.

SEE ALSO
========

:ref:`futhark-run(1)`, :ref:`futhark-test(1)`
