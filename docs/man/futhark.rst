.. role:: ref(emphasis)

.. _futhark(1):

=======
futhark
=======

SYNOPSIS
========

futhark <subcommand> options...

DESCRIPTION
===========

Futhark is a data-parallel functional array language.  Through various
subcommands, the ``futhark`` tool provides facilities for compiling,
developing, or analysing Futhark programs.  Most subcommands are
documented in their own manpage.  For example, ``futhark opencl`` is
documented as :ref:`futhark-opencl(1)`.  The remaining subcommands are
documented below.

COMMANDS
========

futhark benchcmp FILE_A FILE_B
------------------------------

Compare two JSON files produced by the ``--json`` option of
:ref:`futhark-bench(1)`.  The results show speedup of the latter file
compared to the former.

futhark check [-w] [-Werror] PROGRAM
------------------------------------

Check whether a Futhark program type checks. With ``-w``, no warnings
are printed. With ``--Werror``, warnings are treated as errors.

futhark check-syntax PROGRAM
----------------------------

Check whether a Futhark program is syntactically correct.

futhark datacmp FILE_A FILE_B
-----------------------------

Check whether the two files contain the same Futhark values.  The
files must be formatted using the general Futhark data format that is
used by all other executable and tools (such as
:ref:`futhark-dataset(1)`).  All discrepancies will be reported.  This
is in contrast to :ref:`futhark-test(1)`, which only reports the first
one.

futhark dataget PROGRAM DATASET
-------------------------------

Find the test dataset whose description contains ``DATASET``
(e.g. ``#1``) and print it in binary representation to standard
output.  This does not work for ``script`` datasets.

futhark defs PROGRAM
--------------------

Print names and locations of every top-level definition in the program
(including top levels of modules), one per line.  The program need not
be type-correct, but it must not contain syntax errors.

futhark dev options... PROGRAM
------------------------------

A Futhark compiler development command, intentionally undocumented and
intended for use in developing the Futhark compiler, not for
programmers writing in Futhark.

futhark eval [-f FILE] [-w] <exprs...>
--------------------------------------

Evaluates expressions given as command-line arguments. Optionally
allows a file import using ``-f``.

futhark fmt [FILE]
------------------

Reformat the given Futhark program. If no file is provided, read
Futhark program on stdin and produce formatted output on stdout.

futhark hash PROGRAM
--------------------

Print a hexadecimal hash of the program AST, including all non-builtin
imports.  Supposed to be invariant to whitespace changes.

futhark imports PROGRAM
-----------------------

Print all non-builtin imported Futhark files to stdout, one per line.

futhark lsp
-----------

Run an LSP (Language Server Protocol) server for Futhark that
communicates on standard input.  There is no reason to run this by
hand.  It is used by LSP clients to provide editor features.

futhark query PROGRAM LINE COL
------------------------------

Print information about the variable at the given position in the
program.

futhark thanks
--------------

Expresses gratitude.

futhark tokens FILE
-------------------

Print the tokens the given Futhark source file; one per line.

SEE ALSO
========

:ref:`futhark-opencl(1)`, :ref:`futhark-c(1)`, :ref:`futhark-py(1)`, :ref:`futhark-pyopencl(1)`, :ref:`futhark-wasm(1)`, :ref:`futhark-wasm-multicore(1)`, :ref:`futhark-ispc(1)`, :ref:`futhark-dataset(1)`, :ref:`futhark-doc(1)`, :ref:`futhark-test(1)`, :ref:`futhark-bench(1)`, :ref:`futhark-run(1)`, :ref:`futhark-repl(1)`, :ref:`futhark-literate(1)`
