.. _js-api:

JavaScript API Reference
========================

The :ref:`futhark-wasm(1)` and :ref:`futhark-wasm-multicore(1)`
compilers produce JavaScript wrapper code to allow JavaScript programs
to invoke the generated WebAssembly code.  This chapter describes the
API exposed by the wrapper.

First a warning: **the JavaScript API is experimental**.  It may
change incompatibly even in minor versions of the compiler.
