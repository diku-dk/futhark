.. _server-protocol:

Server Protocol
===============

A Futhark program can be compiled to a *server executable*.  Such a
server maintains a Futhark context and presents a line-oriented
interface (over stdin/stdout) for loading and dumping values, as well
as calling the entry points in the program.  The main advantage over
the plain executable interface is that program initialisation is done
only *once*, and we can work with opaque values.

The server interface is not intended for human consumption, but is
useful for writing tools on top of Futhark programs, without having to
use the C API.  Futhark's built-in benchmarking and testing tools use
server executables.

A server executable is started like any other executable, and supports
most of the same command line options (:ref:`executable-options`).

Basics
------

Each command is sent as a *single line* on standard input.  A command
consists of space-separated *words*.  A word is either a sequence of
non-space characters (``foo``), *or* double quotes surrounding a
sequence of non-newline and non-quote characters (``"foo bar"``).

The response is sent on standard output. The server will print ``%%%
OK`` on a line by itself to indicate that a command has finished.  It
will also print ``%%% OK`` at startup once initialisation has
finished.  If initialisation fails, the process will terminate.  If a
command fails, the server will print ``%%% FAILURE`` followed by the
error message, and then ``%%% OK`` when it is ready for more input.
Some output may also precede ``%%% FAILURE``, e.g. logging statements
that occured before failure was detected.  Fatal errors that lead to
server shutdown may be printed to stderr.

Variables
---------

Some commands produce or read variables.  A variable is a mapping from
a name to a Futhark value.  Values can be both transparent (arrays and
primitives), but they can also be *opaque* values.  These can be
produced by entry points and passed to other entry points, but cannot
be directly inspected.

Types
-----

All variables have types, and all entry points accept inputs and
produce outputs of defined types.  The notion of transparent and
opaque types are the same as in the C API: primitives and array of
primitives are directly supported, and everything else is treated as
opaque.  See also :ref:`valuemapping`. When printed, types follow
basic Futhark type syntax *without* sizes (e.g. ``[][]i32``).
Uniqueness is not part of the types, but is indicated with an asterisk
in the ``inputs`` and ``outputs`` commands (see below).

Consumption and aliasing
------------------------

Since the server protocol closely models the C API, the same rules
apply to entry points that consume their arguments (see
:ref:`api-consumption`).  In particular, consumed variables must still
be freed with the ``free`` command - but this is the only operation
that may be used on consumed variables.

Commands
--------

The following commands are supported.

General Commands
~~~~~~~~~~~~~~~~

``types``
.........

Print the names of available types, one per line.

``entry_points``
................

Print the names of available entry points.

``call`` *entry* *o1* ... *oN* *i1* ... *iM*
............................................

Call the given entry point with input from the variables *i1* to *iM*.
The results are stored in *o1* to *oN*, which must not already exist.

``restore`` *file* *v1* *t1* ... *vN* *tN*
..........................................

Load *N* values from *file* and store them in the variables *v1* to
*vN* of types *t1* to *tN*, which must not already exist.

``store`` *file* *v1* ... *vN*
..............................

Store the *N* values in variables *v1* to *vN* in *file*.

``free`` *v1* ... *vN*
......................

Delete the given variables.

``rename`` *oldname* *newname*
..............................

Rename the variable *oldname* to *newname*, which must not already
exist.

``inputs`` *entry*
..................

Print the types of inputs accepted by the given entry point, one per
line.  If the given input is consumed, the type is prefixed by `*`.

``outputs`` *entry*
...................

Print the types of outputs produced by the given entry point, one per
line.  If the given output is guaranteed to be unique (does not alias
any inputs), the type is prefixed by `*`.

``clear``
.........

Clear all internal caches and counters maintained by the Futhark
context.  Corresponds to :c:func:`futhark_context_clear_caches`.

``pause_profiling``
...................

Corresponds to :c:func:`futhark_context_pause_profiling`.

``unpause_profiling``
.....................

Corresponds to :c:func:`futhark_context_unpause_profiling`.

``report``
..........

Corresponds to :c:func:`futhark_context_report`.

``set_tuning_param`` *param* *value*
....................................

Corresponds to :c:func:`futhark_context_config_set_tuning_param`.

``tuning_params`` *entry*
.........................

For each tuning parameters relevant to the given entry point, print
its name, then a space, then its class.

This is similar to on :c:func:`futhark_tuning_params_for_sum`, but
note that this command prints *names* and not *integers*.

``tuning_param_class`` *param*
..............................

Corresponds to :c:func:`futhark_get_tuning_param_class`.

Record Commands
~~~~~~~~~~~~~~~

``fields`` *type*
.................

If the given type is a record, print a line for each field of the
record.  The line will contain the name of the field, followed by a
space, followed by the type of the field.  Note that the type name can
contain spaces.  The order of fields is significant, as it is the one
expected by the ``new_record`` command.

``new`` *v0* *type* *v1* ... *vN*
.................................

Create a new variable *v0* of type *type*, which must be a record type
with *N* fields, where *v1* to *vN* are variables with the
corresponding field types (the expected order is given by the
``fields`` command).

``project`` *to* *from* *field*
...............................

Create a new variable *to* whose value is the field *field* of the
record-typed variable *from*.

Environment Variables
---------------------

``FUTHARK_COMPILER_DEBUGGING``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Turns on debugging output for the server when set to 1.
