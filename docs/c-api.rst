.. _c-api:

C API Reference
===============

A Futhark program ``futlib.fut`` compiled to a C library with the
``--library`` command line option produces two files: ``futlib.c`` and
``futlib.h``.  The API provided in the ``.h`` file is documented in
the following.

The ``.h`` file can be included by a C++ source file to access the
functions (``extern "C"`` is added automatically), but the ``.c`` file
must be compiled with a proper C compiler and the resulting object
file linked with the rest of the program.

Using the API requires creating a *configuration object*, which is
then used to obtain a *context object*, which is then used to perform
most other operations, such as calling Futhark functions.

Most functions that can fail return an integer: 0 on success and a
non-zero value on error, as documented below.  Others return a
``NULL`` pointer.  Use :c:func:`futhark_context_get_error` to get a
(possibly) more precise error message.

Some functions take a C string (``const char*``) as argument.  Unless
otherwise indicated, the string will be copied if necessary, meaning
the argument string can always be modified (or freed) after the
function returns.

.. c:macro:: FUTHARK_BACKEND_foo

   A preprocessor macro identifying that the backend *foo* was used to
   generate the code; e.g. ``c``, ``opencl``, or ``cuda``.  This can
   be used for conditional compilation of code that only works with
   specific backends.

Error codes
-----------

Most errors result in a not otherwise specified nonzero return
code, but a few classes of errors have distinct error codes.

.. c:macro:: FUTHARK_SUCCESS

   Defined as ``0``.  Returned in case of success.

.. c:macro:: FUTHARK_PROGRAM_ERROR

   Defined as ``2``.  Returned when the program fails due to
   out-of-bounds, an invalid size coercion, invalid entry point
   arguments, or similar misuse.

.. c:macro:: FUTHARK_OUT_OF_MEMORY

   Defined as ``3``.  Returned when the program fails to allocate
   memory.  This is (somewhat) reliable only for GPU memory - due to
   overcommit and other VM tricks, you should not expect running out
   of main memory to be reported gracefully.

Configuration
-------------

Context creation is parameterised by a configuration object.  Any
changes to the configuration must be made *before* calling
:c:func:`futhark_context_new`.  A configuration object must not be
freed before any context objects for which it is used.  The same
configuration may *not* be used for multiple concurrent contexts.
Configuration objects are cheap to create and destroy.

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

.. c:function:: int futhark_context_config_set_tuning_param(struct futhark_context_config *cfg, const char *param_name, size_t new_value)

   Set the value of a tuning parameter.  Returns zero on success, and
   non-zero if the parameter cannot be set.  This is usually because a
   parameter of the given name does not exist.  See
   :c:func:`futhark_get_tuning_param_count` and
   :c:func:`futhark_get_tuning_param_name` for how to query which
   parameters are available.  Most of the tuning parameters are
   applied only when the context is created, but some may be changed
   even after the context is active.  At the moment, only parameters
   of class "threshold" may change after the context has been created.
   Use :c:func:`futhark_get_tuning_param_class` to determine the class
   of a tuning parameter.

.. c:function:: int futhark_get_tuning_param_count(void)

   Return the number of available tuning parameters.  Useful for
   knowing how to call :c:func:`futhark_get_tuning_param_name` and
   :c:func:`futhark_get_tuning_param_class`.

.. c:function:: const char* futhark_get_tuning_param_name(int i)

   Return the name of tuning parameter *i*, counting from zero.

.. c:function:: const char* futhark_get_tuning_param_class(int i)

   Return the class of tuning parameter *i*, counting from zero.

.. c:function:: void futhark_context_config_set_cache_file(struct futhark_context_config *cfg, const char *fname)

   Ask the Futhark context to use a file with the designated file as a
   cross-execution cache.  This can result in faster initialisation of
   the program next time it is run.  For example, the GPU backends
   will store JIT-compiled GPU code in this file.

   The cache is managed entirely automatically, and if it is invalid
   or stale, the program performs initialisation from scratch.  There
   is no machine-readable way to get information about whether the
   cache was hit succesfully, but you can enable logging to see what
   happens.

   Pass ``NULL`` to disable caching (this is the default).

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

   Free the context object.  It must not be used again.  You must call
   :c:func:`futhark_context_sync` before calling this function to
   ensure there are no outstanding asynchronous operations still
   running. The configuration must be freed separately with
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

   A human-readable string describing the last error.  Returns
   ``NULL`` if no error has occurred.  It is the caller's
   responsibility to ``free()`` the returned string.  Any subsequent
   call to the function returns ``NULL``, until a new error occurs.

.. c:function:: void futhark_context_set_logging_file(struct futhark_context *ctx, FILE* f)

   Set the stream used to print diagnostics, debug prints, and logging
   messages during runtime.  This is ``stderr`` by default.  Even when
   this is used to re-route logging messages, fatal errors will still
   only be printed to ``stderr``.

.. c:function:: char *futhark_context_report(struct futhark_context *ctx)

   Produce a C string encoding a JSON object with debug and profiling
   information collected during program runtime.  It is the caller's
   responsibility to free the returned string.  It is likely to only
   contain interesting information if
   :c:func:`futhark_context_config_set_debugging` or
   :c:func:`futhark_context_config_set_profiling` has been called
   previously.  Returns ``NULL`` on failure.

.. c:function:: int futhark_context_clear_caches(struct futhark_context *ctx)

   Release any context-internal caches and buffers that may otherwise
   use computer resources.  This is useful for freeing up those
   resources when no Futhark entry points are expected to run for some
   time.  Particularly relevant when using a GPU backend, due to the
   relative scarcity of GPU memory.

Values
------

Primitive types (``i32``, ``bool``, etc) are mapped directly to their
corresponding C type.  The ``f16`` type is mapped to ``uint16_t``,
because C does not have a standard ``half`` type.  This integer
contains the bitwise representation of the ``f16`` value in the IEEE
754 binary16 format.

For each distinct array type of primitives (ignoring sizes), an opaque
C struct is defined.  Arrays of ``f16`` are presented as containing
``uint16_t`` elements.  For types that do not map cleanly to C,
including records, sum types, and arrays of tuples, see
:ref:`opaques`.

All array values share a similar API, which is illustrated here for
the case of the type ``[]i32``.  The creation/retrieval functions are
all asynchronous, so make sure to call :c:func:`futhark_context_sync`
when appropriate.  Memory management is entirely manual.  All values
that are created with a ``new`` function, or returned from an entry
point, *must* at some point be freed manually.  Values are internally
reference counted, so even for entry points that return their input
unchanged, you must still free both the input and the output - this
will not result in a double free.

.. c:struct:: futhark_i32_1d

   An opaque struct representing a Futhark value of type ``[]i32``.

.. c:function:: struct futhark_i32_1d *futhark_new_i32_1d(struct futhark_context *ctx, int32_t *data, int64_t dim0)

   Asynchronously create a new array based on the given data.  The
   dimensions express the number of elements.  The data is copied into
   the new value.  It is the caller's responsibility to eventually
   call :c:func:`futhark_free_i32_1d`.  Multi-dimensional arrays are
   assumed to be in row-major form.  Returns ``NULL`` on failure.

.. c:function:: struct futhark_i32_1d *futhark_new_raw_i32_1d(struct futhark_context *ctx, char *data, int64_t offset, int64_t dim0)

   Create an array based on *raw* data, as well as an offset into it.
   This differs little from :c:func:`futhark_i32_1d` when using the
   ``c`` backend, but when using e.g. the ``opencl`` backend, the
   ``data`` parameter will be a ``cl_mem``.  It is the caller's
   responsibility to eventually call :c:func:`futhark_free_i32_1d`.
   The ``data`` pointer must remain valid for the lifetime of the
   array.  Unless you are very careful, this basically means for the
   lifetime of the context.  Returns ``NULL`` on failure.

.. c:function:: int futhark_free_i32_1d(struct futhark_context *ctx, struct futhark_i32_1d *arr)

   Free the value.  In practice, this merely decrements the reference
   count by one.  The value (or at least this reference) may not be
   used again after this function returns.

.. c:function:: int futhark_values_i32_1d(struct futhark_context *ctx, struct futhark_i32_1d *arr, int32_t *data)

   Asynchronously copy data from the value into ``data``, which must
   point to free memory, allocated by the caller, with sufficient
   space to store the full array.  Multi-dimensional arrays are
   written in row-major form.

.. c:function:: const int64_t *futhark_shape_i32_1d(struct futhark_context *ctx, struct futhark_i32_1d *arr)

   Return a pointer to the shape of the array, with one element per
   dimension.  The lifetime of the shape is the same as ``arr``, and
   must *not* be manually freed.  Assuming ``arr`` is a valid
   object, this function cannot fail.

.. _opaques:

Opaque Values
~~~~~~~~~~~~~

Each instance of a complex type in an entry point (records, nested
tuples, etc) is represented by an opaque C struct named
``futhark_opaque_foo``.  In the general case, ``foo`` will be a hash
of the internal representation.  However, if you insert an explicit
type annotation in the entry point (and the type name contains only
characters valid in C identifiers), that name will be used.  Note that
arrays contain brackets, which are not valid in identifiers.  Defining
a type abbreviation is the best way around this.

The API for opaque values is similar to that of arrays, and the same
rules for memory management apply. You cannot construct them from
scratch (unless they correspond to records or tuples, see
:ref:`records`), but must obtain them via entry points (or
deserialisation, see :c:func:`futhark_restore_opaque_foo`).

.. c:struct:: futhark_opaque_foo

   An opaque struct representing a Futhark value of type ``foo``.

.. c:function:: int futhark_free_opaque_foo(struct futhark_context *ctx, struct futhark_opaque_foo *obj)

   Free the value.  In practice, this merely decrements the reference
   count by one.  The value (or at least this reference) may not be
   used again after this function returns.

.. c:function:: int futhark_store_opaque_foo(struct futhark_context *ctx, const struct futhark_opaque_foo *obj, void **p, size_t *n)

   Serialise an opaque value to a byte sequence, which can later be
   restored with :c:func:`futhark_restore_opaque_foo`.  The byte
   representation is not otherwise specified, and is not stable
   between compiler versions or programs.  It is stable under change
   of compiler backend, but not change of compiler version, or
   modification to the source program (although in most cases the
   format will not change).

   The variable pointed to by ``n`` will always be set to the number
   of bytes needed to represent the value.  The ``p`` parameter is
   more complex:

   * If ``p`` is ``NULL``, the function will write to ``*n``, but not
     actually serialise the opaque value.

   * If ``*p`` is ``NULL``, the function will allocate sufficient
     storage with ``malloc()``, serialise the value, and write the
     address of the byte representation to ``*p``.  The caller gains
     ownership of this allocation and is responsible for freeing it.

   * Otherwise, the serialised representation of the value will be
     stored at ``*p``, which *must* have room for at least ``*n``
     bytes.  This is done asynchronously.

   Returns 0 on success.

.. c:function:: struct futhark_opaque_foo* futhark_restore_opaque_foo(struct futhark_context *ctx, const void *p)

   Asynchronously restore a byte sequence previously written with
   :c:func:`futhark_store_opaque_foo`.  Returns ``NULL`` on failure.
   The byte sequence does not need to have been generated by the same
   program *instance*, but it *must* have been generated by the same
   Futhark program, and compiled with the same version of the Futhark
   compiler.

.. _records:

Records
~~~~~~~

A record is an opaque type (see above) that supports additional
functions to *project* individual fields (read their values) and to
construct a value given values for the fields. An opaque type is a
record if its definition is a record at the Futhark level. Note that a
tuple is simply a record with numeric fields.

The projection and construction functions are equivalent in
functionality to writing entry points by hand, and so serve only to
cut down on boilerplate.  Important things to be aware of:

1. The objects constructed though these functions have their own
   lifetime (like any objects returned from an entry point) and must
   be manually freed, independently of the records from which they are
   projected, or the fields they are constructed from.

2. The objects are however in an *aliasing* relationship with the
   fields or original record.  This means you must be careful when
   passing them to entry points that consume their arguments.  As
   always, you don't have to worry about this if you never write entry
   points that consume their arguments.

The precise functions generated depend on the fields of the record.
The following functions assume a record with Futhark-level type ``type
t = {foo: t1, bar: t2}`` where ``t1`` and ``t2`` are also opaque
types.

.. c:function:: int futhark_new_opaque_t(struct futhark_context *ctx, struct futhark_opaque_t **out, const struct futhark_opaque_t2 *bar, const struct futhark_opaque_t1 *foo);

   Construct a record in ``*out`` which has the given values for the
   ``bar`` and ``foo`` fields.  The parameters are the
   fields in alphabetic order.  Tuple fields are named ``vX`` where
   ``X`` is an integer.  The resulting record *aliases* the values
   provided for ``bar`` and ``foo``, but has its own lifetime, and all
   values must be individually freed when they are no longer needed.

.. c:function:: int futhark_project_opaque_t_bar(struct futhark_context *ctx, struct futhark_opaque_t2 **out, const struct futhark_opaque_t *obj);

   Extract the value of the field ``bar`` from the provided record.
   The resulting value *aliases* the record, but has its own lifetime,
   and must eventually be freed.

.. c:function:: int futhark_project_opaque_t_foo(struct futhark_context *ctx, struct futhark_opaque_t1 **out, const struct futhark_opaque_t *obj);

   Extract the value of the field ``bar`` from the provided record.
   The resulting value *aliases* the record, but has its own lifetime,
   and must eventually be freed.

.. _sums:

Sums
~~~~

A sum type is an opaque type (see above) that supports construction
and destruction functions. An opaque type is a sum type if its
definition is a sum type at the Futhark level.

Similarly to records (see :ref:`Records`), this functionality is
equivalent to writing entry points by hand, and have the same
properties regarding lifetimes.

A sum type consists of one or more variants. A value of this type is
always an instance of one of these variants. In the C API, these
variants are numbered from zero. The numbering is given by the order
in which they are represented in the manifest (see :ref:`manifest`),
which is also the order in which their associated functions are
defined in the header file.

For an opaque sum type ``t``, the following function is always
generated.

.. c:function:: int futhark_variant_opaque_t(struct futhark_context *ctx, const struct futhark_opaque_t *v);

   Return the identifying number of the variant of which this sum type
   is an instance (see above). Cannot fail.

For each variant ``foo``, construction and destruction functions are
defined. The following assume ``t`` is defined as ``type t = #foo
([]i32) bool``.

.. c:function:: int futhark_new_opaque_t_foo(struct futhark_context *ctx, struct futhark_opaque_contrived **out, const struct futhark_i32_1d *v0, const bool v1);

   Construct a value of type ``t`` that is an instance of the variant
   ``foo``. Arguments are provided in the same order as in the
   Futhark-level ``foo`` constructr.

   **Beware:** if ``t`` has size parameters that are only used for
   *other* variants than the one that is being instantiated, those
   size parameters will be set to 0. If this is a problem for your
   application, define your own entry point for constructing a value
   with the proper sizes.

.. c:function:: int futhark_destruct_opaque_contrived_foo(struct futhark_context *ctx, struct futhark_i32_1d **v0, bool *v1, const struct futhark_opaque_contrived *obj);

   Extract the payload of variant ``foo`` from the sum value. Despite
   the name, "destruction" does not free the sum type value. The
   extracted values alias the sum value, but has their own lifetime,
   and must eventually be freed.

   **Precondition:** ``t`` must be an instance of the ``foo`` variant,
   which can be determined with :c:func:`futhark_variant_opaque_t`.


Entry points
------------

Entry points are mapped 1:1 to C functions.  Return values are handled
with *out*-parameters.

For example, this Futhark entry point::

  entry sum = i32.sum

Results in the following C function:

.. c:function:: int futhark_entry_sum(struct futhark_context *ctx, int32_t *out0, const struct futhark_i32_1d *in0)

   Asynchronously call the entry point with the given arguments.  Make
   sure to call :c:func:`futhark_context_sync` before using the value
   of ``out0``.

Errors are indicated by a nonzero return value.  On error, the
*out*-parameters are not touched.

The precise semantics of the return value depends on the backend.  For
the sequential C backend, errors will always be available when the
entry point returns, and :c:func:`futhark_context_sync` will always
return zero.  When using a GPU backend such as ``cuda`` or ``opencl``,
the entry point may still be running asynchronous operations when it
returns, in which case the entry point may return zero successfully,
even though execution has already (or will) fail.  These problems will
be reported when :c:func:`futhark_context_sync` is called.  Therefore,
be careful to check the return code of *both* the entry point itself,
and :c:func:`futhark_context_sync`.

For the rules on entry points that consume their input, see
:ref:`api-consumption`.  Note that even if a value has been consumed,
you must still manually free it.  This is the only operation that is
permitted on a consumed value.

GPU
---

The following API functions are available when using the ``opencl``,
``cuda``, or ``hip`` backends.

.. c:function:: void futhark_context_config_set_device(struct futhark_context_config *cfg, const char *s)

   Use the first device whose name contains the given string.  The
   special string ``#k``, where ``k`` is an integer, can be used to
   pick the *k*-th device, numbered from zero.  If used in conjunction
   with :c:func:`futhark_context_config_set_platform`, only the
   devices from matching platforms are considered.


Exotic
~~~~~~

The following functions are not interesting to most users.

.. c:function:: void futhark_context_config_set_default_thread_block_size(struct futhark_context_config *cfg, int size)

   Set the default number of work-items in a thread block.

.. c:function:: void futhark_context_config_set_default_group_size(struct futhark_context_config *cfg, int size)

   Identical to
   :c:func:`futhark_context_config_set_default_thread_block_size`;
   provided for backwards compatibility.

.. c:function:: void futhark_context_config_set_default_grid_size(struct futhark_context_config *cfg, int num)

   Set the default number of thread blocks used for kernels.

.. c:function:: void futhark_context_config_set_default_num_groups(struct futhark_context_config *cfg, int num)

   Identical to
   :c:func:`futhark_context_config_set_default_grid_size`;
   provided for backwards compatibility.

.. c:function:: void futhark_context_config_set_default_tile_size(struct futhark_context_config *cfg, int num)

   Set the default tile size used when executing kernels that have
   been block tiled.

.. c:function:: const char* futhark_context_config_get_program(struct futhark_context_config *cfg)

   Retrieve the embedded GPU program.  The context configuration keeps
   ownership, so don't free the string.

.. c:function:: void futhark_context_config_set_program(struct futhark_context_config *cfg, const char *program)

   Instead of using the embedded GPU program, use the provided string,
   which is copied by this function.

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

.. c:function:: void futhark_context_config_set_command_queue(struct futhark_context_config *cfg, cl_command_queue queue)

   Use exactly this command queue for the context.  If this is set,
   all other device/platform configuration options are ignored.  Once
   the context is active, the command queue belongs to Futhark and
   must not be used by anything else.  This is useful for
   implementing custom device selection logic in application code.

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

.. c:function:: cl_program futhark_context_get_program(struct futhark_context_config *cfg)

   Retrieve the compiled OpenCL program.

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

.. c:function:: void futhark_context_dump_ptx_to(struct futhark_context_config *cfg, const char *path)

   During :c:func:`futhark_context_new`, dump the generated PTX code
   to the given file.

.. c:function:: void futhark_context_config_load_ptx_from(struct futhark_context_config *cfg, const char *path)

   During :c:func:`futhark_context_new`, read PTX code from the given
   file instead of using the embedded program.

Multicore
---------

The following API functions are available when using the ``multicore``
backend.

.. c:function:: void futhark_context_config_set_num_threads(struct futhark_context_config *cfg, int n)

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

Violation the restrictions of consumption (see :ref:`api-consumption`)
can result in undefined behaviour.  This does not matter for programs
whose entry points do not have unique parameter types
(:ref:`in-place-updates`).

.. _manifest:

Manifest
--------

When compiling with ``--library``, the C backends generate a
machine-readable *manifest* in JSON format that describes the API of
the compiled Futhark program. Specifically, the manifest contains:

* A mapping from the name of each entry point to:

  * The C function name of the entry point.

  * A list of all *inputs*, including their type (as a name) and
    *whether they are unique* (consuming).

  * A list of all *outputs*, including their type (as a name) and
    *whether they are unique*.

  * A list of all *tuning parameters* that can influence the execution
    of this entry point.  These are not necessarily unique to the
    entry point.

* A mapping from the name of each non-scalar type to:

  * The C type used to represent this type (which is in practice
    always a pointer of some kind).

  * What *kind* of type this is - either an *array* or an *opaque*.

  * For arrays, the element type and rank.

  * A mapping from *operations* to the names of the C functions that
    implement the operations for the type.  The types of the C
    functions are as documented above.  The following operations are
    listed:

    * For arrays: ``free``, ``shape``, ``values``, ``new``.

    * For opaques: ``free``, ``store``, ``restore``.

  * For opaques that are actually records (including tuples):

    * The list of fields, including their type and a projection
      function.  The field ordering here is the one expected by
      the *new* function.

    * The name of the C *new* function for creating a record from
      field values.

Manifests are defined by the following JSON Schema:

.. include:: manifest.schema.json
   :code: json

It is likely that we will add more fields in the future, but it is
unlikely that we will remove any.
