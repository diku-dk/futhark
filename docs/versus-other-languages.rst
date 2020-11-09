.. _versus-other-languages:

Futhark Compared to Other Functional Languages
==============================================

This guide is intended for programmers who are familiar with other functional
languages and want to start working with Futhark.

Futhark is a simple language with a complex compiler.
Functional programming is fundamentally well suited to
data-parallelism, so Futhark's syntax and underlying concepts are taken directly
from established functional languages; mostly from Haskell and the
members of the ML family.  While Futhark does add a few small
conveniences (built-in array types) and one complicated and unusual
feature (in-place updates via uniqueness types, see
:ref:`in-place-updates`), a programmer familiar with a common
functional language should be able to understand the meaning of a
Futhark program, and quickly begin writing their own programs.  To
speed up this process, we describe here some of the various
quirks and unexpected limitations imposed by Futhark. We also
recommended reading some of the `example programs`_ along with this guide.
The guide does *not* cover all Futhark features worth knowing, so do also
skim :ref:`language-reference`.

.. _`example programs`: https://github.com/diku-dk/futhark/tree/master/examples

Basic Syntax
------------

Futhark uses a keyword-based structure, with optional indentation
*solely* for human readability.  This aspect differs from Haskell and F#.

Names are lexically divided into *identifiers* and *symbols*:

* *Identifiers* begin with a letter or underscore and contain letters,
  numbers, underscores, and apostrophes.

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
the longest prefix of *op*.  For example, ``<<=`` would have the
same fixity as ``<<``, and ``=<<`` the same as ``=``.  This rule is the
same as the rule found in OCaml and F#.

Top-level functions and values are defined with ``let``, as in OCaml
and F#.

Evaluation
----------

Futhark is a completely pure language, with no cheating through monads
or anything of the sort.

Evaluation is *eager* or *call-by-value*, like most non-Haskell
languages.  However, there is no defined evaluation order.
Furthermore, the Futhark compiler is permitted to turn non-terminating
programs into terminating programs, for example by removing dead code
that might cause an error.  Moreover, there is no way to
handle errors within a Futhark program (no exceptions or similar);
although errors are gracefully reported to whatever invokes the
Futhark program.

The evaluation semantics are entirely sequential, with parallelism
being solely an operational detail.  Hence, race conditions are
impossible. The Futhark compiler does not automatically go
looking for parallelism.  Only certain special constructs and built-in
library functions (in particular ``map``, ``reduce``, ``scan``, and
``filter``) may be executed in parallel.

Currying and partial application work as usual (although functions
are not fully first class; see `Types`_).  Some Futhark language
constructs look like functions, but are not.  This means they cannot
be partially applied.  These include ``unsafe`` and ``assert``.

Lambda terms are written as ``\x -> x + 2``, as in Haskell.

A Futhark program is read top-down, and all functions must be declared
in the order they are used, like Standard ML.  Unlike just
about all functional languages, recursive functions are *not*
supported.  Most of the time, you will use bulk array operations
instead, but there is also a dedicated ``loop`` language construct,
which is essentially syntactic sugar for tail recursive functions.

Types
-----

Futhark supports a range of integer types, floating point types, and
booleans (see :ref:`primitives`).  A numeric literal can be suffixed
with its desired type, such as ``1i8`` for an eight-bit signed
integer.  Un-adorned numerals have their type inferred based on use.
This only works for built-in numeric types.

Arrays are a built-in type.  The type of an array containing elements
of type ``t`` is written ``[]t`` (not ``[t]`` as in Haskell), and we
may optionally annotate it with a size as ``[n]t`` (see `Shape
Declarations`).  Array values are written as ``[1,2,3]``.  Array
indexing is written ``a[i]`` with *no* space allowed between the array
name and the brace.  Indexing of multi-dimensional arrays is written
``a[i,j]``.  Arrays are 0-indexed.

All types can be combined in tuples as usual, as well as in
*structurally typed records*, as in Standard ML.  Non-recursive sum
types are supported, and are also structurally typed.  Abstract types
are possible via the module system; see :ref:`module-system`.

If a variable ``foo`` is a record of type ``{a: i32, b: bool}``, then
we access field ``a`` with dot notation: ``foo.a``.  Tuples are a
special case of records, where all the fields have a 0-indexed numeric
label.  For example, ``(i32, bool)`` is the same as ``{0: i32, 1:
bool}``, and can be indexed as ``foo.1``.

Sum types are defined as constructors separated by a vertical bar
(``|``).  Constructor names always start with a ``#``.  For example,
``#red | #blue i32`` is a sum type with the constructors ``#red`` and
``#blue``, where the latter has an ``i32`` as payload.  The terms
``#red`` and ``#blue 2`` produce values of this type.  Constructor
applications must always be fully saturated.  Due to the structural
typing, type annotations are usually necessary to resolve ambiguities.
For example, the term ``#blue 2`` can produce a value of *any type*
that has an appropriate constructor.

Function types are supported with the usual ``a -> b``, and functions can be
passed as arguments to other functions.  However, there are some
restrictions:

* A function cannot be put in an array (but a record or tuple is
  fine).

* A function cannot be returned from a branch.

* A function cannot be used as a ``loop`` parameter.

Function types interact with type parameters in a subtle way::

  let id 't (x: t) = x

This declaration defines a function ``id`` that has a type parameter
``t``.  Here, ``t`` is an *unlifted* type parameter, which is
guaranteed never to be a function type, and so in the body of the
function we could choose to put parameter values of type ``t`` in an
array.  However, it means that this identity function cannot be called
on a functional value.  Instead, we probably want a *lifted* type
parameter::

  let id '^t (x: t) = x

Such *lifted* type parameters are not restricted from being
instantiated with function types.  On the other hand, in the function
definition they are subject to the same restrictions as functional
types.

Futhark supports Hindley-Milner type inference (with some
restrictions), so we could also just write it as::

  let id x = x

Type abbreviations are possible::

  type foo = (i32, i32)

Type parameters are supported as well::

  type pair 'a 'b = (a, b)

As with everything else, they are structurally typed, so the types
``pair i32 bool`` and ``(i32, bool)`` are entirely interchangeable.
Most unusually, this is also the case for sum types.  The following
two types are entirely interchangeable::

  type maybe 'a = #just a | #nothing

  type option 'a = #nothing | #just a

Only for abstract types, where the definition has been hidden via the
module system, do type names have any significance.

Size parameters can also be passed::

  type vector [n] t = [n]t
  type i32matrix [n][m] = [n] (vector [m] i32)

Note that for an actual array type, the dimensions come *before* the
element type, but with a type abbreviation, a size is just another
parameter.  This easily becomes hard to read if you are not careful.
