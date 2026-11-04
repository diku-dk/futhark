.. _js-api:

JavaScript API Reference
========================

The :ref:`futhark-wasm(1)` and :ref:`futhark-wasm-multicore(1)` compilers can
generate JavaScript wrapper code when used with ``--library``. This chapter
describes the API exposed by those wrappers.

First a warning: **the JavaScript API is experimental**. It may change
incompatibly even in minor versions of the compiler.

A Futhark program ``futlib.fut`` compiled with ``--library`` produces
the following files:

* ``futlib.mjs``: an ES module that exports the JavaScript API.
* ``futlib.wasm``: the compiled WebAssembly module, which is loaded by
  ``futlib.mjs`` and must be in the same directory.
* ``futlib.c`` and ``futlib.class.js``: intermediate files, which are
  not needed at runtime.

The module exports a ``FutharkContext`` class, which is the top-level
interface for using a compiled Futhark program from JavaScript:

.. code-block:: javascript

   import { FutharkContext } from './futlib.mjs';

   const fut = new FutharkContext();
   await fut.init();

In a browser, the module must be loaded by a ``<script
type="module">`` element (or imported by another module), and the
files must be served over HTTP.  In Node.js, a CommonJS module can
load it with ``await import('./futlib.mjs')``.

General concerns
----------------

Memory management is completely manual, as JavaScript does not support
finalizers that could let Futhark hook into the garbage collector.
You are responsible for eventually freeing all objects produced by the
API, using the appropriate methods.

Methods that may run Futhark code are asynchronous, and return a
promise.  The asynchronous operations on a ``FutharkContext`` run one at
a time, in the order they were started, and while one is running, no
synchronous method may be called on the ``FutharkContext`` or its
values.  Errors, including errors that occur while running Futhark
code, are reported by throwing an ``Error`` (or rejecting the promise
with one).  The arguments of every method are checked, and an invalid
argument is reported with a ``TypeError`` or ``RangeError``.

Top-level wrapper objects
-------------------------

The top-level JavaScript interface is ``FutharkContext``.  A
``FutharkContext`` object represents an instance of a compiled Futhark
program, and corresponds to a context in the C API.  It is used to
construct arrays, call entry points, and free resources.

.. js:class:: FutharkContext()

   A bookkeeping class representing an instance of a compiled Futhark
   program.

.. js:function:: FutharkContext.init(options)

   Asynchronously initialise the ``FutharkContext`` object.  This must
   be done exactly once, before the object is used for anything else.
   Each ``FutharkContext`` loads its own instance of the WebAssembly
   module, so values cannot be passed between different
   ``FutharkContext`` objects.

   The ``options`` argument is optional.  If given, it must be an object
   with any of the following fields, which correspond to the
   configuration functions of the C API (see :ref:`c-api`):

   * ``debugging``, ``logging``, ``profiling``: booleans that enable
     debugging, logging, or profiling.  Logging information is printed
     with ``console.error``.

   * ``tuning_params``: an object mapping the names of tuning parameters
     to their values.

   * ``num_threads``: the number of threads to use.  Only supported by
     :ref:`futhark-wasm-multicore(1)`.

   With :ref:`futhark-wasm-multicore(1)`, ``init()`` fails unless
   ``SharedArrayBuffer`` is available, which in a browser requires the
   page to be *cross-origin isolated*.

.. js:function:: FutharkContext.free()

   Frees the underlying Futhark context and configuration.  It is an
   error to use a ``FutharkArray`` or ``FutharkOpaque`` after the
   ``FutharkContext`` on which it was created has been freed.

.. js:attribute:: FutharkContext.entry

   Object containing the entry point functions. For example, an entry
   point named ``main`` is available as ``fut.entry.main``.

.. js:attribute:: FutharkContext.types

   Object mapping the name of each array type used by the program, such
   as ``[]i32``, to its constructor object (see `Array construction`_).

The ``FutharkContext`` also provides utility methods for
synchronisation, cache management, and profiling.  They are all
asynchronous.

.. js:function:: FutharkContext.context_sync()

   Wait for pending work associated with the context to finish.

.. js:function:: FutharkContext.clear_caches()

   Clear caches associated with the context.

.. js:function:: FutharkContext.report()

   Return a profiling report string.

.. js:function:: FutharkContext.pause_profiling()

   Pause profiling.

.. js:function:: FutharkContext.unpause_profiling()

   Resume profiling.

Values
------

Numeric types ``u8``, ``u16``, ``u32``, ``i8``, ``i16``, ``i32``, ``f16``,
``f32``, and ``f64`` are mapped to JavaScript's standard number type.
64-bit integers ``u64`` and ``i64`` are mapped to ``BigInt``, although a
number is also accepted as an argument.  An integer argument must be in
the range of its type.  ``bool`` is mapped to JavaScript's ``boolean``
type.  Arrays are represented by ``FutharkArray`` objects.  Complex
types (records, nested tuples, etc.) are represented by
``FutharkOpaque`` objects.

FutharkArray
------------

Arrays are represented by ``FutharkArray`` objects.

.. js:function:: FutharkArray.shape()

   Returns the shape of the array as a ``BigInt64Array``.

.. js:function:: FutharkArray.values()

   Asynchronously returns a flat typed array, in row-major order,
   containing a copy of the array elements.  It remains valid after the
   ``FutharkArray`` is freed.  The typed array corresponds to the
   element type, such as ``Int32Array`` for ``i32``, except that:

   * For ``f16``, it is a ``Float16Array`` if supported by the
     JavaScript implementation, and otherwise a ``Float32Array``.

   * For ``bool``, it is a ``Uint8Array`` whose elements are 0 or 1.

.. js:function:: FutharkArray.free()

   Frees the memory used by the ``FutharkArray``.

Array construction
~~~~~~~~~~~~~~~~~~

Each array type used by an entry point, including as the field of a
record, is exposed through a generated constructor object.  For
example, for the type ``[]i32``:

.. js:function:: fut.i32_1d.from_data(data, dim0)

   Creates and returns a one-dimensional ``i32`` ``FutharkArray`` from
   a JavaScript ``Array`` or typed array in row-major order, with the
   given shape.  The dimensions can be numbers or ``BigInt``\ s, and
   their product must be the length of ``data``.  The elements must be
   valid entry point arguments of the element type, except that for
   ``bool``, they may also be 0 or 1 (as in the result of
   :js:func:`FutharkArray.values`).

The constructor object for an array type with ``n`` dimensions has a
``from_data`` method that takes ``n`` dimensions.

FutharkOpaque
-------------

Complex types (records, nested tuples, etc) are represented by
``FutharkOpaque``.  They can be passed to and returned by entry point
functions, and the fields of records (including tuples) can be read
by projection, as described below.

.. js:function:: FutharkOpaque.free()

   Frees the memory used by the ``FutharkOpaque``.

Records
~~~~~~~

Records, including tuples, are ``FutharkOpaque`` objects whose fields
can be *projected*.  For each field of a record type, the top-level
wrapper object has a method named after the corresponding projection
function in the C API (see :ref:`records`), without the ``futhark_``
prefix.  For example, the field ``x`` of a record with C type ``struct
futhark_opaque_foo`` is projected as follows:

.. js:function:: fut.project_opaque_foo_x(obj)

   Asynchronously returns the value of field ``x`` of the record
   ``obj``.  An array or opaque field is returned as a new
   ``FutharkArray`` or ``FutharkOpaque``, which must be freed
   independently of ``obj``.

Entry Points
------------

Each entry point in the compiled Futhark program is exposed through the
``entry`` field of the top-level wrapper object. For example, an entry
point named ``main`` can be called as:

.. code-block:: javascript

   const res = await fut.entry.main(in1, ..., inN);

.. js:function:: fut.entry.<entry_point_name>(in1, ..., inN)

   Asynchronously call the entry point with the JavaScript
   representations of its arguments, and return the result.

   An entry point that returns a tuple returns a single
   ``FutharkOpaque``, whose elements can be obtained by projection (see
   `Records`_).

   An argument that the entry point consumes (see
   :ref:`in-place-updates`) can afterwards only be freed, and must not
   also be passed as another argument.
