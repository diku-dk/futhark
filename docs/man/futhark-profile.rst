.. role:: ref(emphasis)

.. _futhark-profile(1):

===============
futhark-profile
===============

SYNOPSIS
========

futhark profile JSONFILES

DESCRIPTION
===========

This tool produces human- and browser-readable profiling information based on
information collected with :ref:`futhark bench<futhark-bench(1)>`.
Futhark has basic support for profiling.
The system can collect information about the run-time behaviour
of the program, and connects it as best it is able to the program source code.
This works best for the GPU backends, and not at all for the sequential
backends. The collected information can then be used to estimate the source of
inefficiencies.

USAGE
=====

The first step is to run :ref:`futhark bench<futhark-bench(1)>` on
your program, while passing ``--profile`` and ``--json``. This will
produce a JSON file containing runtime measurements, as well as
collected profiling information. If you neglect to pass ``--profile``,
the profiling information will be missing. The information in the JSON
file is complete, but it is difficult for humans to read.

The next step is to run ``futhark profile`` on the JSON file. For a JSON file
``prog.json``, this will create a *top level directory* ``prog.prof`` that
contains files with human-readable profiling information. A set of files will be
created for each benchmark dataset. If the original invocation of ``futhark
bench`` included multiple programs, then ``futhark profile`` will create
subdirectories for each program (although all inside the same top level
directory). If the source files passed to ``futhark bench`` are accessible via
the original paths, then the directory will also contain HTML files with
annotated source code.

You can pass multiple JSON files to ``futhark profile``. Each will
produce a distinct top level directory.

Files produced
--------------

Supposing a dataset ``foo`` for an entry point in ``source-file.fut``.
``futhark profile`` will produce the following files and directories in the top
level directory.

* ``foo.log``: the running log produced during execution. Contains many details
  on dynamic behaviour, depending on the exact backend.

* ``foo.summary``: a summary of memory usage and cost centres. For the GPU
  backends, the cost centres are kernel executions and memory copies.

* ``foo.timeline``: a list of all recorded profiling events, in the order in
  which they occurred, along with their runtime and other available information,
  most importantly the source locations.

* ``foo-index.html``: overview file and guide to the other html files.
  Contains explanations for the concepts, links to other pages.
  This is the entry file for profile exploration.

The log file is often too verbose to be useful, but the summary and timeline
should be inspected, even if the latter is sometimes fairly large.

All the other files require a web browser but may provide easier access
to the timing data, since the source resolution and parsing is done already.

Technicalities
--------------

The profiling information, including the log, is collected from a
*final* run performed after all the measured runs.  Profiling
information is not collected during the runs that contribute to the
runtime measurement reported by ``futhark bench``.  However, enabling
profiling may still affect performance, as it changes the
behaviour of the run time system.

Raw reports
-----------

Alternatively, the JSON file passed to ``futhark profile`` may also be a raw
profiling report as produced by the C API function ``futhark_context_report()``.
A directory is still created, but it will only contain a single set of files,
and it will not contain a log.

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
