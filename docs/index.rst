.. Futhark documentation master file, created by
   sphinx-quickstart on Tue Mar 24 14:21:12 2015.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Futhark User's Guide
====================

This document has been prepared as an informal overview of the Futhark
language.  In time, we hope to develop it into a formal specification,
with accompanying proofs of correctness, but for now, words will have
to suffice where formulae would be ideal.

Futhark is an eagerly evaluated, purely functional language with
built-in arrays and second-order array combinators with a focus
towards efficient execution on vector hardware (GPUs).  While Futhark
is not designed to be a pleasant programming experience for humans, it
does have a textual syntax to facilitate the writing of benchmark
programs.

Contents:

.. toctree::
   :maxdepth: 1

   language-overview.rst
   uniqueness-types.rst
   language-reference.rst
   c-porting-guide.rst
   publications.rst

Manual pages:

.. toctree::
   :maxdepth: 1

   man/futhark-c.rst
   man/futhark-opencl.rst
   man/futharki.rst
   man/futhark-test.rst
   man/futhark-bench.rst

Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
