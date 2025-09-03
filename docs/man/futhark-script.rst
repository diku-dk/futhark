.. role:: ref(emphasis)

.. _futhark-script(1):

================
futhark-script
================

SYNOPSIS
========

futhark script [options...] program [expression]

DESCRIPTION
===========

The command ``futhark script foo.fut expr`` will compile ``foo.fut``,
run the provided FutharkScript expression ``expr``, and finally print
the result to stdout. It is essentially a simpler way to access the
evaluation facilities of :ref:`futhark-literate(1)`, and provides the
same FutharkScript facilities, with a few additional built-in
procedures documented below.

If the provided program does not have a ``.fut`` extension, it is
assumed to be a previously compiled server-mode program, and simply
run directly.

When ``-e`` and ``-f`` are used, the expressions are run in the order
provided, and only the value of the last expression is printed. This
implies multiple uses of these options is only useful when they invoke
procedures with side effects.

OPTIONS
=======

--backend=name

  The backend used when compiling Futhark programs (without leading
  ``futhark``, e.g. just ``opencl``).  Defaults to ``c``.

-b, --binary

  Produce output in the binary data format. Fails if the value is not
  a primitive or array of primitives.

-D, --debug

  Pass ``-D`` to the executable and show debug prints.

-e, --expression=EXP

  Evaluate this FutharkScript expression. Expressions are run in the
  order provided.

--futhark=program

  The program used to perform operations (eg. compilation). Defaults
  to the binary running ``futhark script`` itself.

-f, --file=FILe

  Read and evaluate FutharkScript expression from this file.
  Expressions are run in the order provided.

-L, --log

  Pass ``-L`` to the executable and show debug prints.

--pass-option=opt

  Pass an option to benchmark programs that are being run.

--pass-compiler-option=opt

  Pass an extra option to the compiler when compiling the programs.

--skip-compilation

  Do not run the compiler, and instead assume that the program has
  already been compiled.  Use with caution.

-v, --verbose

  Print verbose information on stderr about directives as they are
  executing.  This is also needed to see ``#[trace]`` output.

ADDITIONAL BUILTINS
===================

* ``$store "file" v`` store the value *v* as a binary value in the given file.
  It can be restored again with ``$restore``.

BUGS
====

FutharkScript expressions can only refer to names defined in the file
passed to ``futhark script``, not any names in imported files.

If the result of the expression does not have an external
representation (e.g. is an array of tuples), the value that is printed
is misleading and somewhat nonsensical.

SEE ALSO
========

:ref:`futhark-test(1)`, :ref:`futhark-bench(1)`, :ref:`futhark-literate(1)`
