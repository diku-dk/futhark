.. role:: ref(emphasis)

.. _futhark-webgpu(1):

==============
futhark-webgpu
==============

SYNOPSIS
========

futhark webgpu [options...] <program.fut>

DESCRIPTION
===========

``futhark webgpu`` translates a Futhark program to C code that uses
WebGPU for GPU computation, and then compiles it to WebAssembly using
Emscripten (``emcc``) and its ``emdawnwebgpu`` port.  The result is
meant to be run in a web browser with WebGPU support.

When compiling with ``--library``, the generated files provide the
JavaScript API described in :ref:`js-api`.  When compiling with
``--server``, the generated program implements the Futhark server
protocol (see :ref:`server-protocol`).  It can be run in Node, with
WebGPU provided by the ``webgpu`` npm package, or in a browser that
communicates with a proxy.  The Futhark source repository contains
runners for both, which can be used with :ref:`futhark-test(1)`:
``tools/webgpu/node-runner.sh`` and ``tools/browser_test.py``.

OPTIONS
=======

Accepts the same options as :ref:`futhark-c(1)`.

ENVIRONMENT VARIABLES
=====================

``CFLAGS``

  Space-separated list of options passed to ``emcc``.  Defaults
  to ``-O3 -std=c99`` if unset.

``EMCFLAGS``

  Space-separated list of options passed to ``emcc``.

BUGS
====

The WebGPU backend has various restrictions. Some of these are due to
fundamental limitations in WebGPU and are unlikely to be resolved. The following
are the main restrictions that a Futhark programmer will notice.

* ``f64`` is not supported in parallel code. Programs that use it there fail to
  compile.

* Histograms (``hist`` and ``reduce_by_index``) on 64-bit integers fail to
  compile.

* Some programs that produce arrays with ``if`` or ``loop`` inside parallel code
  fail to compile.

* Errors in parallel code, such as out-of-bounds indexing, division by zero, and
  failed assertions, are not detected. The program instead produces unspecified
  results.

* Programs only run on devices that support the ``shader-f16`` feature, even if
  they do not use ``f16``.

* Tuning files are not supported, and are silently ignored.

SEE ALSO
========

:ref:`futhark-c(1)`, :ref:`futhark-wasm(1)`
