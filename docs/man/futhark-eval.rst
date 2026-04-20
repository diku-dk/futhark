.. role:: ref(emphasis)

.. _futhark-eval(1):

================
futhark-eval
================

SYNOPSIS
========

futhark eval [options...] expression

DESCRIPTION
===========

This command executes Futhark expressions through the interpreter and prints
their results to stdout. You can provide a ``.fut`` file with definitions that
are then accessible in the expression.

Further, if you pass ``--backend``, the file will be compiled with the given
compiler backend, and any references to entry points in the evaluation
expression will be handled by compiled code. This is the main purpose of
``futhark eval``: it allows convenient invocation of Futhark entry points
without having to recompile.

The expression itself is not compiled, so it is best to put as much of the heavy
work as possible into the entry points provided in the ``.fut`` file. Also,
beware: while the expression can access any definition in the file, only the
ones explicitly declared as entry points will be compiled and run fast. All
other definitions will be executed in interpreted mode.

OPTIONS
=======

--backend=name

  The backend used when compiling Futhark programs (without leading ``futhark``,
  e.g. just ``opencl``). Defaults to no backend, meaning purely interpreted.

-f, --file=FILE

  Evaluate expressions in the context of this file.

--futhark=program

  The program used to perform operations (eg. compilation).  Defaults
  to the binary running ``futhark eval`` itself.

--pass-option=opt

  Pass an option to benchmark programs that are being run.  For
  example, we might want to run OpenCL programs on a specific device::

    futhark eval prog.fut --backend=opencl --pass-option=-dHawaii

--pass-compiler-option=opt

  Pass an extra option to the compiler when compiling the programs.

--skip-compilation

  Do not run the compiler, and instead assume that the program has
  already been compiled.  Use with caution.

SEE ALSO
========

:ref:`futhark-literate(1)`, :ref:`futhark-test(1)`, :ref:`futhark-bench(1)`
