.. role:: ref(emphasis)

.. _futhark-literate(1):

================
futhark-literate
================

SYNOPSIS
========

futhark literate [options...] program

DESCRIPTION
===========

The command ``futhark literate foo.fut`` will compile the given
program and then generate a Markdown file ``foo.md`` that contains a
prettyprinted form of the program.  This is useful for demonstrating
programming techniques.

* Top-level comments that start with a line comment marker (``--``)
  and a space in the next column will be turned into ordinary text in
  the Markdown file.

* Ordinary top-level definitions will be enclosed in Markdown code
  blocks.

* Any *directives* will be executed and replaced with their output.
  See below.

**Warning:** Do not run untrusted programs.  See SAFETY below.

Image directives and builtin functions shell out to ``convert`` (from
ImageMagick).  Video generation uses ``ffmpeg``.

OPTIONS
=======

--backend=name

  The backend used when compiling Futhark programs (without leading
  ``futhark``, e.g. just ``opencl``).  Defaults to ``c``.

--futhark=program

  The program used to perform operations (eg. compilation).  Defaults
  to the binary running ``futhark literate`` itself.

--output=FILE

  Override the default output file.  The image directory will be set
  to the provided ``FILE`` with its extension stripped and ``-img/``
  appended.

--pass-option=opt

  Pass an option to benchmark programs that are being run.  For
  example, we might want to run OpenCL programs on a specific device::

    futhark literate prog.fut --backend=opencl --pass-option=-dHawaii

--pass-compiler-option=opt

  Pass an extra option to the compiler when compiling the programs.

--skip-compilation

  Do not run the compiler, and instead assume that the program has
  already been compiled.  Use with caution.

--stop-on-error

  Terminate immediately without producing an output file if a
  directive fails.  Otherwise a file will still be produced, and
  failing directives will be followed by an error message.

-v, --verbose

  Print verbose information on stderr about directives as they are
  executing.

DIRECTIVES
==========

A directive is a way to show the result of running a function.
Depending on the directive, this can be as simple as printing the
textual representation of the result, or as complex as running an
external plotting program and referencing a generated image.

Any directives that produce images for a program ``foo.fut`` will
place them in the directory ``foo-img/``.  If this directory already
exists, it will be deleted.

A directive is a line starting with ``-- >``, which must follow an
empty line.  Arguments to the directive follow on the remainder of the
line.  Any expression arguments are given in a very restricted subset
of Futhark called *FutharkScript* (see below).

Some directives take mandatory or optional parameters.  These are
entered after a semicolon *and a linebreak*.

The following directives are supported:

* ``> e``

  Shows the result of executing the FutharkScript expression ``e``,
  which can have any (transparent) type.

* ``> :video e[; parameters...]``

  Creates a video from ``e``.  The optional parameters are lines of
  the form *key: value*:

  * ``repeat: <true|false>``

  * ``fps: <int>``

  * ``format: <webm|gif>``

  ``e`` must be one of the following:

  * A 3D array where the 2D elements is of a type acceptable to
    ``:img``, and the outermost dimension is the number of frames.

  * A triple ``(s -> (img,s), s, i64)``, for some types ``s`` and
    ``img``, where ``img`` is an array acceptable to ``:img``.  This
    means not all frames have to be held in memory at once.

* ``> :brief <directive>``

  The same as the given *directive* (which must not start with another
  ``>``), but suppress parameters when printing it.

* ``> :covert <directive>``

  The same as the given *directive* (which must not start with another
  ``>``), but do not show the directive itself in the output, only its
  result.

* ``> :img e``

  Visualises ``e``. The following types are supported:

  * ``[][]i32`` and ``[][]u32``

    Interpreted as ARGB pixel values.

  * ``[][]f32`` and ``[][]f64``

    Interpreted as greyscale. Values should be between 0 and 1, with 0
    being black and 1 being white.

  * ``[][]u8``

    Interpreted as greyscale. 0 is black and 255 is white.

  * ``[][]bool``

    Interpreted as black and white. ``false`` is black and ``true`` is
    white.

* ``> :plot2d e[; size=(height,width)]``

  Shows a plot generated with ``gnuplot`` of ``e``, which must be an
  expression of type ``([]t, []t)``, where ``t`` is some numeric type.
  The two arrays must have the same length and are interpreted as
  ``x`` and ``y`` values, respectively.

  The expression may also be a record expression (*not* merely the
  name of a Futhark variable of record type), where each field will be
  plotted separately and must have the type mentioned above.

* ``> :gnuplot e; script...``

  Similar to ``plot2d``, except that it uses the provided Gnuplot
  script.  The ``e`` argument must be a record whose fields are tuples
  of one-dimensional arrays, and the data will be available in
  temporary files whose names are in variables named after the record
  fields.  Each file will contain a column of data for each array in
  the corresponding tuple.

  Use ``set term png size width,height`` to change the size to
  ``width`` by ``height`` pixels.

FUTHARKSCRIPT
=============

Only an extremely limited subset of Futhark is supported:

.. productionlist::
   script_exp:   `fun` `script_exp`*
            : | "(" `script_exp` ")"
            : | "(" `script_exp` ( "," `script_exp` )+ ")"
            : | "[" `script_exp` ( "," `script_exp` )+ "]"
            : | "empty" "(" ("[" `decimal` "]" )+ `script_type` ")"
            : | "{" "}"
            : | "{" (`id` = `script_exp`) ("," `id` = `script_exp`)* "}"
            : | "let" `script_pat` "=" `script_exp` "in" `script_exp`
            : | `literal`
   script_pat:  `id` | "(" `id` ("," `id`) ")"
   script_fun:  `id` | "$" `id`
   script_type: `int_type` | `float_type` | "bool"

Note that empty arrays must be written using the ``empty(t)``
notation, e.g. ``empty([0]i32)``.

Function applications are either of Futhark functions or *builtin
functions*.  The latter are prefixed with ``$`` and are magical
(usually impure) functions that could not possibly be implemented in
Futhark.  The following builtins are supported:

* ``$loadimg "file"`` reads an image from the given file and returns
  it as a row-major ``[][]u32`` array with each pixel encoded as ARGB.

SAFETY
======

Some directives (e.g. ``:gnuplot``) can run arbitrary shell commands.
Other directives or builtin functions can read or write arbitrary
files.  Running an untrusted literate Futhark program is as dangerous
as running a shell script you downloaded off the Internet.  Before
running a program from an unknown source, you should always give it a
quick read to see if anything looks fishy.

SEE ALSO
========

:ref:`futhark-test(1)`, :ref:`futhark-bench(1)`
