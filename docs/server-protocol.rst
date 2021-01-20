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
most of the same command line options.

Basics
------

Each command is sent as a *single line* on standard input.  The
response is sent on standard output.  The server will print ``%%% OK``
on a line by itself to indicate that a command has finished.  If a
command fails, the server will print ``%%% FAILURE`` followed by the
error message, and then ``%%% OK`` when it is ready for more input.
Some output may also precede ``%%% FAILURE``, e.g. logging statements
that occured before failure was detected.  Fatal errors (that lead to
server shutdown) may be printed to stderr.

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
opaque.  When printed, types follow basic Futhark type syntax
*without* sizes (e.g. ``[][]i32``).

Commands
--------

The following commands are supported.

``call`` *entry* *o1* ... *oN* *i1* ... *oM*
............................................

Call the given entry point with input from the variables *i1* to *oM*.
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

``inputs`` *entry*
..................

Print the types of inputs accepted by the given entry point, one per
line.

``outputs`` *entry*
...................

Print the types of outputs produced by the given entry point, one per
line.

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
