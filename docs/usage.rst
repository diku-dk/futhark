.. _usage:

Basic Usage
===========

Futhark contains several code generation backends.  Each is provided
as a full standalone compiler binary.  For example, ``futhark-c``
compiles a Futhark program by translating it to sequential C code,
while ``futhark-pyopencl`` generates Python and the PyOpenCL library.
The different compilers all contain the same frontend and optimisation
pipeline - only the code generator is different.  They all provide
roughly the same command line interface, but there may be minor
differences and quirks due to characteristics of the specific
backends.

There are two main ways of compiling a Futhark program: to an
executable, and to a library.  Executables can be run immediately, but
are useful mostly for testing and benchmarking.  Libraries can be
integrated with non-Futhark code.

Compiling to Executable
-----------------------

A Futhark program is stored in a file with the extension ``.fut``.  It
can be compiled to an executable program as follows::

  $ futhark-c prog.fut

This makes use of the ``futhark-c`` compiler, but any other will work
as well.  The result is an executable binary called ``prog``.  If we
had used ``futhark-py`` instead of ``futhark-c``, the ``prog`` file
would instead have contained Python code, along with a `shebang`_ for
easy execution.  In general, when compiling file ``foo.fut``, the
result will be written to a file ``foo`` (i.e. the extension will be
stripped off).  This can be overridden using the ``-o`` option.

When a Futhark program is run, execution starts at the function named
``main``.  An alternative entry point can be indicated by using the
``-e`` option.  All entry point functions must be declared
appropriately in the program (see :ref:`entry-points`).  If the entry
point takes any parameters, these will be read from standard input in
a subset of the Futhark syntax.  A binary input format is also
supported; see :ref:`binary-data-format`.

Only a subset of all Futhark values can be passed to an executable.
Specifically, only primitives and arrays of primitive types are
supported.  In particular, nested tuples and arrays of tuples are not
permitted.  Non-nested tuples are supported are supported as simply
flat values.  This restriction is not present for Futhark programs
compiled to libraries.  If an entry point *returns* any such value,
its printed representation is unspecified.  As a special case, an
entry point is allowed to return a flat tuple.

Instead of compiling, there is also an interpreter, ``futharki``.  Be
aware that it is very slow, and does not produce better error messages
than the compiler.  **Note:** If you run ``futharki`` without any
options, you will see something that looks deceptively like a `REPL`_,
but it is not yet finished, and only marginally useful in its present
state.

Compiling to Library
--------------------

While compiling a Futhark program to an executable is useful for
testing, it is not suitable for production use.  Instead, a Futhark
program should be compiled into a reusable library in some target
language, enabling integration into a larger program.  At the moment,
this work has only been done for the ``futhark-py`` and
``futhark-pyopencl`` backends, and only the latter of these generates
code of sufficient performance to be worthwhile.

We can use ``futhark-pyopencl`` to translate the program
``futlib.fut`` into a Python module ``futlib.py`` with the following
command::

  futhark-pyopencl --library futlib.fut

This will create a file ``futlib.py``, which contains Python code that
defines a class named ``futlib``.  This class defines one method for
each entry point function (see :ref:`entry-points`) in the Futhark
program.  After the class has been instantiated, these methods can be
invoked to run the corresponding Futhark function.  The reason for why
the module does does not expose functions directly, is that some
complicated initialisation and stateful bookkeeping may be necessary
for sophisticated backends, such as PyOpenCL.

.. _shebang: https://en.wikipedia.org/wiki/Shebang_%28Unix%29
.. _REPL: https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop
