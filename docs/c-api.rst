.. _c-api:

C API Reference
===============

A Futhark program ``futlib.fut`` compiled to a C library with the
``--library`` command line option produces two files: ``futlib.c`` and
``futlib.h``.  The API provided in the ``.h`` file is documented in
the following.

Using the API requires creating a *configuration object*, which is
then used to obtain a *context object*, which is then used to perform
most other operations, such as calling Futhark functions.

Most functions that can fail return an integer: 0 on success and a
non-zero value on error.  Others return a ``NULL`` pointer.  Use
:c:func:`futhark_context_get_error` to get a (possibly) more precise
error message.

.. c:macro:: FUTHARK_BACKEND_foo

   A preprocessor macro identifying that the backend *foo* was used to
   generate the code; e.g. ``c``, ``opencl``, or ``cuda``.  This can
   be used for conditional compilation of code that only works with
   specific backends.

Configuration
-------------

Context creation is parameterised by a configuration object.  Any
changes to the configuration must be made *before* calling
:c:func:`futhark_context_new`.  A configuration object must not be
freed before any context objects for which it is used.  The same
configuration may be used for multiple concurrent contexts.

.. c:struct:: futhark_context_config

   An opaque struct representing a Futhark configuration.

.. c:function:: struct futhark_context_config *futhark_context_config_new(void)

   Produce a new configuration object.  You must call
   :c:func:`futhark_context_config_free` when you are done with
   it.

.. c:function:: void futhark_context_config_free(struct futhark_context_config *cfg)

   Free the configuration object.

.. c:function:: void futhark_context_config_set_debugging(struct futhark_context_config *cfg, int flag)

   With a nonzero flag, enable various debugging information, with the
   details specific to the backend.  This may involve spewing copious
   amounts of information to the standard error stream.  It is also
   likely to make the program run much slower.

.. c:function:: void futhark_context_config_set_profiling(struct futhark_context_config *cfg, int flag)

   With a nonzero flag, enable the capture of profiling information.
   This should not significantly impact program performance.  Use
   :c:func:`futhark_context_report` to retrieve captured information,
   the details of which are backend-specific.

.. c:function:: void futhark_context_config_set_logging(struct futhark_context_config *cfg, int flag)

   With a nonzero flag, print a running log to standard error of what
   the program is doing.

Context
-------

.. c:struct:: futhark_context

   An opaque struct representing a Futhark context.

.. c:function:: struct futhark_context *futhark_context_new(struct futhark_context_config *cfg)

   Create a new context object.  You must call
   :c:func:`futhark_context_free` when you are done with it.  It is
   fine for multiple contexts to co-exist within the same process, but
   you must not pass values between them.  They have the same C type,
   so this is an easy mistake to make.

   After you have created a context object, you must immediately call
   :c:func:`futhark_context_get_error`, which will return non-``NULL``
   if initialisation failed.  If initialisation has failed, then you
   still need to call :c:func:`futhark_context_free` to release
   resources used for the context object, but you may not use the
   context object for anything else.

.. c:function:: void futhark_context_free(struct futhark_context *ctx)

   Free the context object.  It must not be used again.  The
   configuration must be freed separately with
   :c:func:`futhark_context_config_free`.

.. c:function:: int futhark_context_sync(struct futhark_context *ctx)

   Block until all outstanding operations, including copies, have
   finished executing.  Many API functions are asynchronous on their
   own.

.. c:function:: void futhark_context_pause_profiling(struct futhark_context *ctx)

   Temporarily suspend the collection of profiling information.  Has
   no effect if profiling was not enabled in the configuration.

.. c:function:: void futhark_context_unpause_profiling(struct futhark_context *ctx)

   Resume the collection of profiling information.  Has no effect if
   profiling was not enabled in the configuration.

.. c:function:: char *futhark_context_get_error(struct futhark_context *ctx)

   A human-readable string describing the last error, if any.  It is
   the caller's responsibility to ``free()`` the returned string.  Any
   subsequent call to the function returns ``NULL``, until a new error
   occurs.

.. c:function:: char *futhark_context_report(struct futhark_context *ctx)

   Produce a human-readable C string with debug and profiling
   information collected during program runtime.  It is the caller's
   responsibility to free the returned string.  It is likely to only
   contain interesting information if
   :c:func:`futhark_context_config_set_debugging` or
   :c:func:`futhark_context_config_set_profiling` has been called
   previously.

.. c:function:: int futhark_context_clear_caches(struct futhark_context *ctx)

   Release any context-internal caches and buffers that may otherwise
   use computer resources.  This is useful for freeing up those
   resources when no Futhark entry points are expected to run for some
   time.  Particularly relevant when using a GPU backend, due to the
   relative scarcity of GPU memory.

Values
------

Primitive types (``i32``, ``bool``, etc) are mapped directly to their
corresponding C type.  For each distinct array type of primitives
(ignoring sizes), an opaque C struct is defined.  For types that do
not map cleanly to C, including records, sum types, and arrays of
tuples, see :ref:`opaques`.

All array values share a similar API, which is illustrated here for
the case of the type ``[]i32``.  The creation/retrieval functions are
all asynchronous, so make sure to call :c:func:`futhark_context_sync`
when appropriate.  Memory management is entirely manual.  All values
that are created with a ``new`` function, or returned from an entry
point, *must* at some point be freed manually.  Values are internally
reference counted, so even for entry points that return their input
unchanged, you should still free both the input and the output - this
will not result in a double free.

.. c:struct:: futhark_i32_1d

   An opaque struct representing a Futhark value of type ``[]i32``.

.. c:function:: struct futhark_i32_1d *futhark_new_i32_1d(struct futhark_context *ctx, int32_t *data, int64_t dim0)

   Asynchronously create a new array based on the given data.  The
   dimensions express the number of elements.  The data is copied into
   the new value.  It is the caller's responsibility to eventually
   call :c:func:`futhark_free_i32_1d`.  Multi-dimensional arrays are
   assumed to be in row-major form.

.. c:function:: struct futhark_i32_1d *futhark_new_raw_i32_1d(struct futhark_context *ctx, char *data, int offset, int64_t dim0)

   Create an array based on *raw* data, as well as an offset into it.
   This differs little from :c:func:`futhark_i32_1d` when using the
   ``c`` backend, but when using e.g. the ``opencl`` backend, the
   ``data`` parameter will be a ``cl_mem``.  It is the caller's
   responsibility to eventually call :c:func:`futhark_free_i32_1d`.

.. c:function:: int futhark_free_i32_1d(struct futhark_context *ctx, struct futhark_i32_1d *arr)

   Free the value.  In practice, this merely decrements the reference
   count by one.  The value (or at least this reference) may not be
   used again after this function returns.

.. c:function:: int futhark_values_i32_1d(struct futhark_context *ctx, struct futhark_i32_1d *arr, int32_t *data)

   Asynchronously copy data from the value into ``data``, which must
   be of sufficient size.  Multi-dimensional arrays are written in
   row-major form.

.. c:function:: const int64_t *futhark_shape_i32_1d(struct futhark_context *ctx, struct futhark_i32_1d *arr)

   Return a pointer to the shape of the array, with one element per
   dimension.  The lifetime of the shape is the same as ``arr``, and
   should *not* be manually freed.

.. _opaques:

Opaque values
~~~~~~~~~~~~~

Each instance of a complex type in an entry point (records, nested
tuples, etc) is represented by an opaque C struct named
``futhark_opaque_foo``.  In the general case, ``foo`` will be a hash
of the internal representation.  However, if you insert explicit type
annotations in the entry point (and the type name contains only
characters valid for C identifiers), the indicated name will be used.
Note that arrays contain brackets, which are usually not valid in
identifiers.  Defining a simple type abbreviation is the best way
around this.

The API for opaque values is similar to that of arrays, and the same
rules for memory management apply.  You cannot construct them from
scratch, but must obtain them via entry points (or deserialisation,
see :c:func:`futhark_restore_opaque_foo`).

.. c:struct:: futhark_opaque_foo

   An opaque struct representing a Futhark value of type ``foo``.

.. c:function:: int futhark_free_opaque_foo(struct futhark_context *ctx, struct futhark_opaque_foo *obj)

   Free the value.  In practice, this merely decrements the reference
   count by one.  The value (or at least this reference) may not be
   used again after this function returns.

.. c:function:: int futhark_store_opaque_foo(struct futhark_context *ctx, const struct futhark_opaque_foo *obj, void **p, size_t *n);

   Serialise an opaque value to a byte sequence, which can later be
   restored with :c:func:`futhark_restore_opaque_foo`.  The byte
   representation is not otherwise specified.  The variable pointed to
   by ``n`` will always be set to the number of bytes needed to
   represent the value.  The ``p`` parameter is more complex:

   * If ``p`` is ``NULL``, the function will write to ``*n``, but not
     actually serialise the opaque value.

   * If ``*p`` is ``NULL``, the function will allocate sufficient
     storage with ``malloc()``, serialise the value, and write the
     address of the byte representation to ``*p``.

   * Otherwise, the serialised representation of the value will be
     stored at ``*p``, which *must* have room for at least ``*n``
     bytes.

   Returns 0 on success.

.. c:function:: struct futhark_opaque_foo* futhark_restore_opaque_foo(struct futhark_context *ctx, const void *p);

   Restore a byte sequence previously written with
   :c:func:`futhark_store_opaque_foo`.  Returns ``NULL`` on failure.
   The byte sequence does not need to have been generated by the same
   program *instance*, but it *must* have been generated by the same
   Futhark program, and compiled with the same version of the Futhark
   compiler.

Entry points
------------

Entry points are mapped 1:1 to C functions.  Return values are handled
with *out*-parameters.

For example, this Futhark entry point::

  entry sum = i32.sum

Results in the following C function:

.. c:function:: int futhark_entry_main(struct futhark_context *ctx, int32_t *out0, const struct futhark_i32_1d *in0)

   Asynchronously call the entry point with the given arguments.  Make
   sure to call :c:func:`futhark_context_sync` before using the value
   of ``out0``.

The exact behaviour of the exit code depends on the backend.  For the
sequential C backend, errors will always be available when the entry
point returns, and :c:func:`futhark_context_sync` will always return
success.  When using a GPU backend such as ``cuda`` or ``opencl``, the
entry point may still be running asynchronous operations when it
returns, in which case the entry point may return zero successfully,
even though execution has already (or will) fail.  These problems will
be reported when :c:func:`futhark_context_sync` is called.  When using
GPU backends, be careful to check the return code of *both* the entry
point itself, and :c:func:`futhark_context_sync`.

GPU
---

The following API functions are available when using the ``opencl`` or
``cuda`` backends.

.. c:function:: void futhark_context_config_set_device(struct futhark_context_config *cfg, const char *s)

   Use the first device whose name contains the given string.  The
   special string ``#k``, where ``k`` is an integer, can be used to
   pick the *k*-th device, numbered from zero.  If used in conjunction
   with :c:func:`futhark_context_config_set_platform`, only the
   devices from matching platforms are considered.


Exotic
~~~~~~

The following functions are not interesting to most users.

.. c:function:: void futhark_context_config_set_default_group_size(struct futhark_context_config *cfg, int size)

   Set the default number of work-items in a work-group.

.. c:function:: void futhark_context_config_set_default_num_groups(struct futhark_context_config *cfg, int num)

   Set the default number of work-groups used for kernels.

.. c:function:: void futhark_context_config_set_default_tile_size(struct futhark_context_config *cfg, int num)

   Set the default tile size used when executing kernels that have
   been block tiled.

.. c:function:: void futhark_context_config_dump_program_to(struct futhark_context_config *cfg, const char *path)

   During :c:func:`futhark_context_new`, dump the OpenCL or CUDA
   program source to the given file.

.. c:function:: void futhark_context_config_load_program_from(struct futhark_context_config *cfg, const char *path)

   During :c:func:`futhark_context_new`, read OpenCL or CUDA program
   source from the given file instead of using the embedded program.

OpenCL
------

The following API functions are available only when using the
``opencl`` backend.

.. c:function:: void futhark_context_config_set_platform(struct futhark_context_config *cfg, const char *s)

   Use the first OpenCL platform whose name contains the given string.
   The special string ``#k``, where ``k`` is an integer, can be used
   to pick the *k*-th platform, numbered from zero.

.. c:function:: void futhark_context_config_select_device_interactively(struct futhark_context_config *cfg)

   Immediately conduct an interactive dialogue on standard output to
   select the platform and device from a list.

.. c:function:: struct futhark_context *futhark_context_new_with_command_queue(struct futhark_context_config *cfg, cl_command_queue queue)

   Construct a context that uses a pre-existing command queue.  This
   allows the caller to directly customise which device and platform
   is used.

.. c:function:: cl_command_queue futhark_context_get_command_queue(struct futhark_context *ctx)

   Retrieve the command queue used by the Futhark context.  Be very
   careful with it - enqueueing your own work is unlikely to go well.

Exotic
~~~~~~

The following functions are used for debugging generated code or
advanced usage.

.. c:function:: void futhark_context_config_add_build_option(struct futhark_context_config *cfg, const char *opt)

   Add a build option to the OpenCL kernel compiler.  See the OpenCL
   specification for `clBuildProgram` for available options.

.. c:function:: void futhark_context_config_dump_binary_to(struct futhark_context_config *cfg, const char *path)

   During :c:func:`futhark_context_new`, dump the compiled OpenCL
   binary to the given file.

.. c:function:: void futhark_context_config_load_binary_from(struct futhark_context_config *cfg, const char *path)

   During :c:func:`futhark_context_new`, read a compiled OpenCL binary
   from the given file instead of using the embedded program.

CUDA
----

The following API functions are available when using the ``cuda``
backend.

Exotic
~~~~~~

The following functions are used for debugging generated code or
advanced usage.

.. c:function:: void futhark_context_config_add_nvrtc_option(struct futhark_context_config *cfg, const char *opt)

   Add a build option to the NVRTC compiler.  See the CUDA
   documentation for ``nvrtcCompileProgram`` for available options.

.. c:function:: void futhark_context_config_dump_ptx_to(struct futhark_context_config *cfg, const char *path)

   During :c:func:`futhark_context_new`, dump the generated PTX code
   to the given file.

.. c:function:: void futhark_context_config_load_ptx_from(struct futhark_context_config *cfg, const char *path)

   During :c:func:`futhark_context_new`, read PTX code from the given
   file instead of using the embedded program.

Multicore
---------

The following API functions are available when using the ``multicore``
backend.

.. c:function:: void context_config_set_num_threads(struct futhark_context_config *cfg, int n)

   The number of threads used to run parallel operations.  If set to a
   value less than ``1``, then the runtime system will use one thread
   per detected core.


General guarantees
------------------

Calling an entry point, or interacting with Futhark values through the
functions listed above, has no system-wide side effects, such as
writing to the file system, launching processes, or performing network
connections.  Defects in the program or Futhark compiler itself can
with high probability result only in the consumption of CPU or GPU
resources, or a process crash.

Using the ``#[unsafe]`` attribute with in-place updates can result in
writes to arbitrary memory locations.  A malicious program can likely
exploit this to obtain arbitrary code execution, just as with any
insecure C program.  If you must run untrusted code, consider using
the ``--safe`` command line option to instruct the compiler to disable
``#[unsafe]``.

Initialising a Futhark context likewise has no side effects, except if
explicitly configured differently, such as by using
:c:func:`futhark_context_config_dump_program_to`.  In its default
configuration, Futhark will not access the file system.

Note that for the GPU backends, the underlying API (such as CUDA or
OpenCL) may perform file system operations during startup, and perhaps
for caching GPU kernels in some cases.  This is beyond Futhark's
control.
