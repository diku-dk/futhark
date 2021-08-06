.. _binary-data-format:

Binary Data Format
==================

Futhark programs compiled to an executable support both textual and binary
input. Both are read via standard input, and can be mixed, such that one
argument to an entry point may be binary, and another may be textual. The binary
input format takes up significantly less space on disk, and can be read much
faster than the textual format. This chapter describes the binary input format
and its current limitations. The input formats (whether textual or binary) are
not used for Futhark programs compiled to libraries, which instead use whichever
format is supported by their host language.

Currently reading binary input is only supported for compiled programs.
It is *not* supported for ``futhark run``.

You can generate random data in the binary format with ``futhark
dataset`` (:ref:`futhark-dataset(1)`).  This tool can also be used to
convert between binary and textual data.

Futhark-generated executables can be asked to generate binary output
with the ``-b`` option.

Specification
-------------

Elements that are bigger than one byte are always stored using little endian --
we mostly run our code on x86 hardware so this seemed like a reasonable choice.

When reading input for an argument to the entry function, we need to be able to
differentiate between text and binary input. If the first non-whitespace
character of the input is a ``b`` we will parse this argument as binary,
otherwise we will parse it in text format. Allowing preceding whitespace
characters makes it easy to use binary input for some arguments, and text input
for others.

The general format has this header::

  b <version> <num_dims> <type> <values...>

Where ``version`` is a byte containing the version of the binary format used for
encoding (currently 2), ``num_dims`` is the number of dimensions in the array as
a single byte (0 for scalar), and ``type`` is a 4 character string describing
the type of the values(s) -- see below for more details.

Encoding a scalar value is done by treating it as a 0-dimensional array::

  b <version> 0 <type> <value>

To encode an array, we encode the number of dimensions ``n`` as a
single byte, each dimension ``dim_i`` as an unsigned 64-bit little
endian integer, and finally all the values in row-major order in their
binary little endian representation::

  b <version> <n> <type> <dim_1> <dim_2> ... <dim_n> <values...>


Type Values
~~~~~~~~~~~

A type is identified by a 4 character ASCII string (four bytes). Valid
types are::

  "  i8"
  " i16"
  " i32"
  " i64"
  "  u8"
  " u16"
  " u32"
  " u64"
  " f16"
  " f32"
  " f64"
  "bool"

Note that unsigned and signed integers have the same byte-level
representation.

Values of type ``bool`` are encoded with a byte each.  The results are
undefined if this byte is not either 0 or 1.
