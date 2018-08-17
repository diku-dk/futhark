.. role:: ref(emphasis)

.. _futhark-cs(1):

=========
futhark-cs
=========

SYNOPSIS
========

futhark-cs [-V] [-o outfile] infile

DESCRIPTION
===========

``futhark-cs`` translates a Futhark program to sequential C# code, and
either compiles that C# code with the Roslyn C# Compiler ``csc``
to an executable binary program, or produces a ``.dll`` file that can be linked with
other code..  The standard Futhark optimisation pipeline is used, and
``csc`` is invoked with ``-lib:$MONO_PATH``, ``-r:Cloo.clSharp.dll``,
``-r:Mono.Options.dll``, and ``/unsafe``.

The resulting program will read the arguments to the entry point
(``main`` by default) from standard input and print its return value
on standard output.  The arguments are read and printed in Futhark
syntax, just like futharki(1).

OPTIONS
=======

-o outfile
  Where to write the result.  If the source program is named
  'foo.fut', this defaults to 'foo'.

--library
  Generate a library instead of an executable.  Appends ``.dll``
  to the name indicated by the ``-o`` option to determine output
  file names.

-v verbose
  Enable debugging output.  If compilation fails due to a compiler
  error, the result of the last successful compiler step will be
  printed to standard error.

-h
  Print help text to standard output and exit.

-V
  Print version information on standard output and exit.

REQUIREMENTS
===========
``futhark-cs`` uses the Mono implementation of the .NET framework.
To compile and execute the compiled binaries/libraries, you must have the ``MONO_PATH`` environment variable defined. ``MONO_PATH`` must be set to a directory containing the ``Mono.Options`` and ``Cloo.clSharp`` dll's.

Mono.Options is available on https://www.nuget.org/packages/Mono.Options/5.3.0.1

Cloo.clSharp is available on https://www.nuget.org/packages/Cloo.clSharp/


SEE ALSO
========

futharki(1), futhark-test(1)
