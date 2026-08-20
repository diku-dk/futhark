.. role:: ref(emphasis)

.. _futhark-repl(1):

============
futhark-repl
============

SYNOPSIS
========

futhark repl [options...] [program.fut]

DESCRIPTION
===========

Start an interactive Futhark session. This will let you interactively enter
expressions and declarations which are then immediately interpreted. If the
entered line can be either a declaration or an expression, it is assumed to be a
declaration. The input must fit on a single line. Evaluation uses the Futhark
interpreter, which is somewhat slow, but allows use of the ``#[break]``
attribute.

Futhark source files can be loaded using the ``:load`` command.  This
will erase any interactively entered definitions.  Use the ``:help``
command to see a list of commands.  All commands are prefixed with a
colon.

If ``--backend`` is passed, the entry points of the loaded program are not
interpreted, but are instead compiled and run by a server-mode executable.
Everything else - including any definitions entered interactively - is still
interpreted. This lets you inspect the results of compiled code interactively,
at the cost of the entry points no longer being subject to interpreter
facilities such as ``#[break]``. Values produced by an entry point are fetched
from the server only when needed, so indexing a large result does not require
transferring all of it.

OPTIONS
=======

--backend=name

  Run the entry points of the loaded program with this backend
  (without leading ``futhark``, e.g. just ``opencl``), rather than
  interpreting them.

-h
  Print help text to standard output and exit.

--pass-compiler-option=opt

  Pass an extra option to the compiler when compiling the program.

-p, --pass-option=opt

  Pass an option to the server-mode executable that is being run.

--skip-compilation

  Do not run the compiler, and instead assume that the program has
  already been compiled into a server-mode executable.  Use with
  caution.

-V
  Print version information on standard output and exit.

SEE ALSO
========

:ref:`futhark-run(1)`, :ref:`futhark-test(1)`
