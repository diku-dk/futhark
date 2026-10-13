.. role:: ref(emphasis)

.. _futhark-benchcmp(1):

================
futhark-benchcmp
================

SYNOPSIS
========

futhark benchcmp [options...] FILE_A FILE_B

DESCRIPTION
===========

Compare two JSON files produced by the ``--json`` option of
:ref:`futhark-bench(1)` and print a human-readable summary of the
speedup of ``FILE_B`` relative to ``FILE_A``.  A speedup greater than
1x means ``FILE_B`` is faster; a speedup less than 1x means ``FILE_B``
is slower (a regression).

Results are grouped by program and entry point.  Within each group the
datasets are listed in alphabetical order.  A speedup is highlighted in
green when it is statistically significant and faster, and in red when
it is statistically significant and slower.  Significance is determined
by comparing the difference in means against the sum of half-standard-
deviations of the two samples.

``FILE_A`` is conventionally the *baseline* and ``FILE_B`` the *new*
result.  The typical workflow is::

  futhark bench --backend=cuda --json baseline.json prog.fut
  # ... make changes ...
  futhark bench --backend=cuda --json new.json prog.fut
  futhark benchcmp baseline.json new.json

OPTIONS
=======

--sort-by=METRIC

  Sort program groups by the given metric.  The default is unsorted
  (alphabetical by program name, matching the order of ``futhark
  bench`` output).

  ``significant``
    Sort by the number of datasets in the group that have a
    statistically significant regression (speedup < 0.99).  Groups with
    the most regressions appear first.

  ``geomean-significant``
    Sort by the geometric mean of speedups restricted to statistically
    significant datasets.  Groups whose significant datasets are slowest
    on average appear first.  Groups with no significant results are
    treated as 1.0x (no change) for sorting purposes.

  ``geomean-all``
    Sort by the geometric mean of speedups across *all* datasets in the
    group, regardless of significance.  Groups that are slowest on
    average appear first.

--order=ORDER

  Control the sort direction.  Only meaningful when ``--sort-by`` is
  also given.

  ``worst-first`` (default)
    Surface the most regressed programs at the top.  For
    ``significant`` this means the highest count first; for the geomean
    metrics it means the lowest ratio first (since a ratio below 1
    indicates a slowdown).

  ``best-first``
    Surface the most improved programs at the top.

EXAMPLES
========

Compare two benchmark runs and show the worst regressions first by
number of significant datasets::

  futhark benchcmp --sort-by=significant baseline.json new.json

Show the same comparison ordered by the geometric mean over all
datasets, with the most improved programs first::

  futhark benchcmp --sort-by=geomean-all --order=best-first baseline.json new.json

SEE ALSO
========

:ref:`futhark-bench(1)`
