.. _hacking:

Hacking on the Futhark Compiler
===============================

The Futhark compiler is a significant body of code with a not entirely
straightforward design.  The main source of documentation is the
Haddock comments in the source code itself.  You can generate
hyperlinked reference documentation by running ``stack haddock`` or
``cabal haddock``, depending on your preference of build system.
There is also possibly-outdated `documentation on Hackage`_

If you feel that the documentation is incomplete, or something lacks
an explanation, then feel free to report it as an issue on the `GitHub
page`_.  Documentation bugs are bugs too.

.. _`documentation on Hackage`: http://hackage.haskell.org/package/futhark
.. _`GitHub page`: https://github.com/diku-dk/futhark

The Futhark compiler is usually built using `Stack`_.  It's a good
idea to familiarise yourself with how it works.  As a starting point,
here are a few hints:

  * When testing, pass ``--fast`` to ``stack`` to disable the GHC
    optimiser.  This speeds up builds considerably (although it still
    takes a while).  The resulting Futhark compiler will run slower,
    but it is not something you will notice for small test programs.

  * When debugging, pass ``--profile`` to ``stack``.  This will build
    the Futhark compiler with debugging information (not just
    profiling).  In particular, hard crashes will print a stack trace.
    You can also get actual profiling information by passing
    ``+RTS -pprof-all -RTS`` to the Futhark compiler.  This asks the
    Haskell runtime to print profiling information to a file.  For
    more information, see the `Profiling`_ chapter in the GHC User
    Guide.

  * You may wish to set the environment variable
    ``FUTHARK_COMPILER_DEBUGGING=1``.  This has the following
    effects:

    * The frontend prints internal names.  (This may affect code
      generation in some cases, so turn it off when actually
      generating code.)

    * Tools that talk to server-mode executables will print the
      messages sent back and worth on the standard error stream.

.. _`stack`: https://docs.haskellstack.org/en/stable/README/
.. _`Profiling`: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html

Debugging Internal Type Errors
------------------------------

The Futhark compiler uses a typed core language, and the type checker
is run after every pass.  If a given pass produces a program with
inconsistent typing, the compiler will report an error and abort.
While not every compiler bug will manifest itself as a core language
type error (unfortunately), many will.  To write the erroneous core
program to a file in case of type error, pass ``-v filename`` to the
compiler.  This will also enable verbose output, so you can tell which
pass fails.  The ``-v`` option is also useful when the compiler itself
crashes, as you can at least tell where in the pipeline it got to.

Checking Generated Code
-----------------------

Hacking on the compiler will often involve inspecting the quality of
the generated code.  The recommended way to do this is to use
:ref:`futhark-c(1)` or :ref:`futhark-opencl(1)` to compile a Futhark
program to an executable.  These backends insert various forms of
instrumentation that can be enabled by passing run-time options to the
generated executable.

  * As a first resort, use ``-t`` option to use the built-in runtime
    measurements.  A nice trick is to pass ``-t /dev/stderr``, while
    redirecting standard output to ``/dev/null``.  This will print the
    runtime on the screen, but not the execution result.

  * Optionally use ``-r`` to ask for several runs, e.g. ``-r 10``.  If
    combined with ``-t``, this will cause several runtimes to be
    printed (one per line).  The :ref:`futhark-bench(1)` tool itself
    uses ``-t`` and ``-r`` to perform its measurements.

  * Pass ``-D`` to have the program print information on allocation
    and deallocation of memory.

  * (:ref:`futhark-opencl(1)` only) Use the ``-D`` option to enable
    synchronous execution.  ``clFinish()`` will be called after most
    OpenCL operations, and a running log of kernel invocations will be
    printed.  At the end of execution, the program prints a table
    summarising all kernels and their total runtime and average
    runtime.

Using ``futhark dev``
---------------------

For debugging specific compiler passes, the ``futhark dev`` subcommand
allows you to tailor your own compilation pipeline using command line
options.  It is also useful for seeing what the AST looks like after
specific passes.

Running compiler pipelines
--------------------------

You can run the various compiler passes in whatever order you wish.
There are also various shorthands for running entire standard
pipelines:

* ``--gpu``: pipeline used for GPU backends (stopping just
  before adding memory information).

* ``--gpu-mem``: pipeline used for GPU backends, with memory
  information.  This will show the IR that is passed to ImpGen.

* ``--seq``: pipeline used for sequential backends (stopping just
  before adding memory information).

* ``--seq-mem``: pipeline used for sequential backends, with memory
  information.  This will show the IR that is passed to ImpGen.

* ``--mc``: pipeline used for multicore backends (stopping just
  before adding memory information).

* ``--mc-mem``: pipeline used for multicore backends, with memory
  information.  This will show the IR that is passed to ImpGen.

By default, ``futhark dev`` will print the resulting IR.  You can
switch to a different *action* with one of the following options:

* ``--compile-imperative``: generate sequential ImpCode and print it.

* ``--compile-imperative-kernels``: generate GPU ImpCode and print it.

* ``--compile-imperative-multicore``: generate multicore ImpCode and print it.

You must use the appropriate pipeline as well (e.g. ``--gpu-mem`` for
``--compile-imperative-kernels``).

When you are about to have a bad day
------------------------------------

When using the ``cuda`` backend, you can use the ``--dump-ptx``
runtime option to dump PTX, a kind of high-level assembly for NVIDIA
GPUs, corresponding to the GPU kernels.  This can be used to
investigate why the generated code isn't running as fast as you expect
(not fun), or even whether NVIDIAs compiler is miscompiling something
(extremely not fun).  With the OpenCL backend,
``--dump-opencl-binary`` does the same thing.

On AMD platforms, ``--dump-opencl-binary`` tends to produce an actual
binary of some kind, and it is pretty tricky to obtain a debugger for
it (they are available and open source, but the documentation and
installation instructions are terrible).  Instead, AMDs OpenCL kernel
compiler accepts a ``-save-temps=foo`` build option, which will make
it write certain intermediate files, prefixed with ``foo``.  In
particular, it will write an ``.s`` file that contains what appears to
be HSA assembly (at least when using ROCm).  If you find yourself
having to do do this, then you are definitely going to have a bad day,
and probably evening and night as well.
