.. role:: ref(emphasis)

.. _futhark-doc(1):

===========
futhark-doc
===========

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

Futhark definitions may be documented by prefixing them with a block
of line comments starting with :literal:`-- |` (see example below).
Simple Markdown syntax is supported within these comments.  A link to
another identifier is possible with the notation
:literal:`\`name\`@namespace`, where ``namespace`` must be either
``term``, ``type``, or ``mtype`` (module names are in the ``term``
namespace).  A file may contain a leading documentation comment, which
will be considered the file *abstract*.

``futhark-doc`` will ignore any file whose documentation comment
consists solely of the word "ignore".  This is useful for files that
contain tests, or are otherwise not relevant to the reader of the
documentation.

OPTIONS
=======

-o outdir
  The name of the directory that will contain the generated
  documentation.  This option is mandatory.

-v, --verbose
  Print status messages to stderr while running.

-h
  Print help text to standard output and exit.

-V
  Print version information on standard output and exit.

EXAMPLES
========

.. code-block:: futhark

 -- | Gratuitous re-implementation of `map`@term.
 --
 -- Does exactly the same.
 let mymap = ...

SEE ALSO
========

futhark-test(1), futhark-bench(1)
