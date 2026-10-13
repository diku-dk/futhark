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

Entry points that take 64-bit integer arguments may fail if they
need to wait for the GPU, for example to read back a result.  This is
due to a limitation in Emscripten's support for asynchronous
operations.

SEE ALSO
========

:ref:`futhark-c(1)`, :ref:`futhark-wasm(1)`
