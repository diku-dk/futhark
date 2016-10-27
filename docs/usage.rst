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

When a Futhark program is run, execution starts at a the function
named ``main``.  If the program has no ``main`` function, the compiler
will fail with an error.  If ``main`` takes any parameters, these will
be read from standard input in Futhark syntax.  **Note:** Tuple value
syntax is not supported.  Instead, pass the values comprising the
tuple as if they were immediate parameters to the ``main`` function.
We recommended using only primitive types and arrays of primitive
types as parameter (and return) types in the ``main`` function.

Instead of compiling, there is also an interpreter, ``futharki``.  Be
aware that it is very slow, and does not produce better error messages
than the compiler.  **Note:** If you run ``futharki`` without any
options, you will see something that looks deceptively like a `REPL`_,
but it is not yet finished, and useless in its present state.

Compiling to Library
--------------------

While compiling a Futhark program to an executable is useful for
testing, it is not the intended use case.  Instead, a Futhark program
should be compiled into a reusable library in some target language,
enabling integration into a larger program.  At the moment, this work
has only been done for the ``futhark-py`` and ``futhark-pyopencl``
backends, and only the latter of these generates sufficiently
performant code for it to be worthwhile.

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
