.. role:: ref(emphasis)

.. _futhark-profile(1):

===============
futhark-profile
===============

SYNOPSIS
========

futhark profile JSONFILE

DESCRIPTION
===========

This tool produces human-readable profiling information based on
information collected with :ref:`futhark bench<futhark-bench(1)>`.
Futhark has only rudimentary support for profiling.  While the system
can collect information about the run-time behaviour of the program,
there is currently no automatic way to connect the information to the
program source code.  However, the collected information can still be
useful for estimating the source of inefficiencies.

USAGE
=====

The first step is to run :ref:`futhark bench<futhark-bench(1)>` on
your program, while passing ``--profile`` and ``--json``.  This will
produce a JSON file containing runtime measurements, as well as
collected profiling information.  If you neglect to pass
``--profile``, the latter will be missing.  If you neglect to pass
``--json``, no file will be created.  The information in the JSON file
is complete, but it is difficult for humans to read.

The next step is to run ``futhark profile`` on the JSON file.  For a
JSON file ``prog.json``, this will create a *top level directory*
``prog.prof`` that contains files with human-readable profiling
information.  A set of files will be created for each benchmark
dataset.  If the original invocation of ``futhark bench`` included
multiple programs, then ``futhark prof`` will create subdirectories
for each program (although all inside the same top level directory).

Files produced
--------------

Supposing a dataset ``foo``, ``futhark profile`` will produce the
following files in the top level directory.

* ``foo.log``: the running log produced during execution.  Contains
  many details on dynamic behaviour, depending on the exact backend.

* ``foo.summary``: a summary of memory usage and cost centres.  For
  the GPU backends, the cost centres are kernel executions and memory
  copies.

Technicalities
--------------

The profiling information, including the log, is collected from a
*final* run performed after all the measured runs.  Profiling
information is not collected during the runs that contribute to the
runtime measurement reported by ``futhark bench``.  However, enabling
profiling may still affect performance, as it changes the
behaviour of the run time system.

EXAMPLES
========

This shows the sequence of commands one might use to profile the
program ``LocVolCalib.fut``, which has three datasets associated with
it, using the ``hip`` backend::

 $ futhark bench --backend=hip --profile --json result.json LocVolCalib.fut
 $ futhark profile result.json
 $ tree result.prof/
 result.prof/
 ├── LocVolCalib-data_large.in.log
 ├── LocVolCalib-data_large.in.summary
 ├── LocVolCalib-data_medium.in.log
 ├── LocVolCalib-data_medium.in.summary
 ├── LocVolCalib-data_small.in.log
 └── LocVolCalib-data_small.in.summary

BUGS
====

Only the C-based backends currently support profiling.

The ``c`` backend does not actually record useful profiling information.

SEE ALSO
========

:ref:`futhark-bench(1)`
