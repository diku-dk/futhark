.. role:: ref(emphasis)

.. _futhark-script(1):

==============
futhark-script
==============

SYNOPSIS
========

futhark script [options...] program

DESCRIPTION
===========

The command ``futhark script foo.fut`` will compile the given program
and then generate a Markdown file ``foo.md`` that contains a
prettyprinted form of the program.  This is useful for illustrative
example programs.

* Top-level comments that start with a line comment marker (``--``)
  and a space in the first column will be turned into ordinary text in
  the Markdown file.

* Ordinary top-level definitions will be enclosed in Markdown code
  blocks.

* Any *directives* will be executed and replaced with their output.
  See below.

Directives
==========

A directive is a way to embed the result of a *FutharkScript*
expression in the program.  Depending on the directive, this can be as
simple as printing the textual representation of the result, or as
complex as running an external plotting program and referencing a
generated image.

Any directives that produce images for a program ``foo.fut`` will
place them in the directory ``foo-img/``.

A directive is a line starting with ``-- >``, which must follow an
empty line.  Arguments to the directive follow on the remainder of the
line.  Any expression arguments are given in a very restricted subset
of Futhark called *FutharkScript* (see below).

The following directives are supported:

* ``> e``

  Shows the result of executing the FutharkScript expression ``e``,
  which can have any (transparent) type.

* ``> :img e``

  Visualises ``e``, which must be of type ``[][]i32`` or ``[][]u32``
  (interpreted as rows of ARGB pixel values).  Shells out to
  ``convert`` (from ImageMagick) to generate the image.

* ``> :plot2d [<height,width>] e``

  Shows a plot generated with ``gnuplot`` of ``e``, which must be an
  expression of type ``([]t, []t)``, where ``t`` is some numeric type.
  The two arrays must have the same length and are interpreted as
  ``x`` and ``y`` values, respectively.

  The expression may also be a record, where each field will be
  plotted separately and must have the type mentioned above.

* ``> :gnuplot e;\n script...``

  There *must* be a newline after the semicolon.  Similar to
  ``plot2d``, except that it uses the provided Gnuplot script.  The
  ``e`` argument must be a record whose fields are tuples of
  one-dimensional arrays, and the data will be available in temporary
  files whose names are in variables named after the record fields.
  Use ``set term png size width,height`` to change the size to
  ``width`` by ``height`` pixels.

FutharkScript
=============

Only an extremely limited subset of Futhark is supported:

.. productionlist::
   scriptexp:   `id` `scriptexp`*
            : | "(" `scriptexp` ")"
            : | "(" `scriptexp` ( "," `scriptexp` )+ ")"
            : | "{" "}"
            : | "{" (`id` = `scriptexp`) ("," `id` = `scriptexp`)* "}"
            : | `literal`

Any numeric literals *must* have a type suffix.

OPTIONS
=======

--backend=name

  The backend used when compiling Futhark programs (without leading
  ``futhark``, e.g. just ``opencl``).  Defaults to ``c``.

--futhark=program

  The program used to perform operations (eg. compilation).  Defaults
  to the binary running ``futhark script`` itself.

--output=FILE

  Override the default output file.  The image directory will be set
  to the provided ``FILE`` with its extension stripped and ``-img/``
  appended.

--pass-option=opt

  Pass an option to benchmark programs that are being run.  For
  example, we might want to run OpenCL programs on a specific device::

    futhark script prog.fut --backend=opencl --pass-option=-dHawaii

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


SEE ALSO
========

:ref:`futhark-test(1)`, :ref:`futhark-bench(1)`
