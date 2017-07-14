.. role:: ref(emphasis)

.. _futhark-doc(1):

=========
futhark-doc
=========

SYNOPSIS
========

futhark-doc [-o outdir] dir

DESCRIPTION
===========

``futhark-doc`` generates HTML-formatted documentation from Futhark
code.  One HTML file will be created for each ``.fut`` file in the
given directory, as well as any file reachable through ``import``
expressions.  The given Futhark code will be considered as one
cohesive whole, and must be type-correct.

OPTIONS
=======

-o outdir
  The name of the directory that will contain the generated
  documentation.  This option is mandatory.

-h
  Print help text to standard output and exit.

-V
  Print version information on standard output and exit.

SEE ALSO
========

futhark-test(1), futhark-bench(1)
