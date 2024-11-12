.. role:: ref(emphasis)

.. _futhark-fmt(1):

===========
futhark-fmt
===========

SYNOPSIS
========

futhark fmt [FILES]

DESCRIPTION
===========

Reformat the given Futhark programs. If no files are provided, read
Futhark program on stdin and produce formatted output on stdout. If
stdout is a terminal, the output will be syntax highlighted.

In contrast to many other automatic formatters, the formatting is
somewhat sensitive to the formatting of the input program. In
particular, single-line expressions will usually be kept on a single
line, even if they are very long. To force ``futhark fmt`` to break
these, insert a linebreak at an arbitrary location.

OPTIONS
=======

-h
  Print help text to standard output and exit.

-V
  Print version information on standard output and exit.

SEE ALSO
========

:ref:`futhark-doc(1)`
