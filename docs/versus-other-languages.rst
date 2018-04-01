.. _versus-other-languages:

Futhark Compared to Other Functional Languages
==============================================

This guide is intended to quickly get programmers familiar with other
functional languages acquainted with Futhark.

Futhark is intended to be a simple language with a complex compiler.
Functional programming is fundamentally well suited to
data-parallelism, so of its concepts and syntax are taken directly
from established functional languages; mostly from Haskell and the
members of the ML familiy.  While Futhark does add a few small
conveniences (built-in array types) and one complicated and unusual
feature (in-place updates via uniqueness types, see
:ref:`in-place-updates`), a programmer familiar with a common
functional language should be able to easily deduce the meaning of a
Futhark program, and quickly start writing their own programs.  To
speed up this process, the following describes some of the various
quirks and unexpected limitations imposed by Futhark.  It is
recommended to read some of the `example programs`_ along this guide.
This is *not* a list of all Futhark features worth knowing, so do also
skim the `Language Reference`.

.. _`example programs`: https://github.com/diku-dk/futhark/tree/master/examples

Basic Syntax
------------

Futhark uses a keyword-based structure, with optional indentation
*solely* for human readability.  This differs from Haskell and F#.

Names are lexically divided into *identifiers* and *symbols*:

* *Identifiers* begin with a letter and contain letters, numbers, underscores
  and apostrophes.

* *Symbols* contain the characters found in the default operators
  (``+-*/%=!><|&^``)

All function and variable names must be identifiers, and all infix
operators are symbols.  An identifier can be used as an infix operator
by enclosing it in backticks, as in Haskell.

Identifiers are case-sensitive, and there is no restriction on the
case of the first letter (unlike Haskell and OCaml, but like Standard
ML).

User-defined operators are possible, but the fixity of the operator
depends on its name.  Specifically, the fixity of a user-defined
operator *op* is equal to the fixity of the built-in operator that is
the longest prefix of *op*.  So, for example, ``<<=`` would have the
same fixity as ``<<``,, and ``=<<`` the same as ``<``.  This is the
same rule as in OCaml and F#.

Top-level functions and values are defined with ``let``, as in OCaml
and F#.

Evaluation
----------

Futhark is a completely pure language, with no cheating through monads
or anything of the sort.

Evaluation is *eager* or *call-by-value*, like most non-Haskell
languages.  However, there is no defined evaluation order.
Furthermore, the Futhark compiler is permitted to turn non-terminating
programs into terminating programms, for examply by removing dead code
that might cause an error.  This also means that there is no way to
handle errors within a Futhark program (no exceptions or similar);
although errors are gracefully reported to whatever invokes the
Futhark program.

The evaluation semantics are entirely sequential, with parallelism
being solely an operational detail.  Hence, race conditions are
impossible.  However, the Futhark compiler does not automatically go
looking for parallelism.  Only certain special constructs and built-in
library functions (in particular ``map``, ``reduce``, ``scan``, and
``filter``) may be executed in parallel.

Currying and partial application works as usual (although functions
are not fully first class; see `Types`_).

Lambda terms are written as ``\x -> x + 2``, as in Haskell.

A Futhark program is read top-down, and all functions must be declared
in the order they are used, similarly to Standard ML.  Unlike just
about all functional languages, recursive functions are *not*
supported.  Most of the time you will use bulk array operations
instead, but there is also a dedicated ``loop`` language construct,
which is essentially syntactic sugar for tail recursive functions.

Types
-----

Futhark supports a range of integer types, floating point types, and
booleans (see :ref:`primitives`).  A numeric literal can be suffixed
with its desired type, e.g. ``1i8`` for an eight-bit signed integer.
Un-adorned integers have type ``i32`` and un-adorned decimal numbers
have ``f64`` (double precision float).

All types can be combined in tuples as usual, as well as in
*structurally typed records*, as in Standard ML.  There are not yet
any sum types.  Abstract types are possible via the module system; see
:ref:`module-system`.

If a variable ``foo`` is a record of type ``{a: i32, b: bool}``, then
we access field ``a`` with dot notation: ``foo.a``.  Tuples are a
special case of records, where all the fields have a 1-indexed numeric
label.  For example, ``(i32, bool)`` is the same as ``{1: i32, 2:
bool}``, and can be indexed as ``foo.1``.

Arrays are a built-in type.  The type of an array containing elements
of type ``t`` is written ``[]t`` (not ``[t]`` as in Haskell), and we
may optionally annotate it with a size as ``[n]t`` (see `Shape
Declarations`).  Array values are written as ``[1,2,3]``.  Array
indexing is ``a[i]`` but there must *not* be a space between the array
name and the brace.  Indexing of multi-dimensional arrays is
``a[i,j]``.

Function types are supported with the usual ``a -> b``, and can be
passed as arguments to other functions.  However, there are some
restrictions:

* A function cannot be put it an array (but a record or tuple is
  fine).

* A function cannot be returned from a branch.

* A function cannot be used as a ``loop`` parameter.

This interacts with type parameters in a subtle way::

  let id 't (x: t) = x

This specifies a function ``id`` that has a type parameter ``t``.
Here, ``t`` is an *unlifted* type parameter, which is guaranteed to
never be a function, and so in the body of the function we could put
it in an array, if we wish.  However, it means that this identity
function cannot be called on a functional value.  Instead, we probably
want a *lifted* type parameter::

  let id '^t (x: t) = x

Futhark supports Hindley-Milner type inference (with some
restrictions), so we could also just write it as::

  let id x = x

Type appreviations are possible::

  type foo = (i32, i32)

Type parameters are supported as well::

  type pair 'a 'b = (a, b)

As with everything else, they are structurally typed, so the types
``pair i32 bool`` and ``(i32, bool)`` are entirely interchangeable.

Size parameters can also be passed::

  type vector [n] t = [n]t
  type i32matrix [n][m] = [n] (vector [m] i32)

Note that for an actual array type, the dimensions come *before* the
element type, but with a type abbreviation, a size is just another
parameter.  This easily becomes hard to read if you are not careful.
