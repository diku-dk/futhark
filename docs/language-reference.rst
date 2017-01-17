.. _language-reference:

Language Reference
==================

This Futhark language reference manual seeks to describe every
language construct.  It is not presented in a tutorial fashion, but
rather intended for quick lookup and documentation of subtleties.  For
this reason, it is not written in a bottom-up manner, and some
concepts may be used before they are fully defined.  It is a good idea
to have a basic grasp of Futhark (or some other functional programming
language) before reading this reference.

Primitive Types and Values
--------------------------

The primitive types in Futhark are the signed integer types ``i8``,
``i16``, ``i32``, ``i64``, the unsigned integer types ``u8``, ``u16``,
``u32``, ``u64``, the floating-point types ``f32``, ``f64``, as well
as ``bool``.  An ``f32`` is always a single-precision float and a
``f64`` is a double-precision float.  All primitive types can be
combined in tuples and arrays.

Numeric literals can be suffixed with their intended type.  For
example ``42i8`` is of type ``i8``, and ``1337e2f64`` is of type
``f64``.  If no suffix is given, integer literals are of type ``i32``,
and decimal literals are of type ``f64``.  Hexadecimal literals are
supported by prefixing with ``0x``, and binary literals by prefixing
with ``0b``.

Numeric values can be converted between different types by using the
desired type name as a function.  E.g., ``i32(1.0f32)`` would convert
the floating-point number ``1.0`` to a 32-bit signed integer.
Conversion from floating-point to integers is done by truncation.

Boolean literals are written ``true`` and ``false``.  These can also
be converted to numbers (1 for true, 0 for false) by using the desired
numeric type as a function.

Character and string literals are supported, but only as an alias for
integers and arrays of integers, respectively.  There is no character
data type.

Function Declarations
---------------------

A function declaration must specify the name, parameters, return
type, and body of the function::

  fun name(params...): rettype = body

Type inference is not supported, and functions are fully monomorphic.
If the function is neither recursive or referenced before it is
defined, the return type may be elided.  Optionally, the programmer
may put *shape declarations* in the return type and parameter types.
These can be used to express invariants about the shapes of arrays
that are accepted or produced by the function, e.g::

  fun f(a: [n]i32): [n]i32 =
    map(+1, a)

The above declaration specifies a function that takes an array
containing ``n`` elements and returns an array likewise containing
``n`` elements.  In general, shape declarations in parameters are
fresh names, whilst shape declarations in return types must refer to a
name of type ``i32`` in scope.  A shape declaration can also be an
integer constant (with no suffix).

The same name can be used in several dimensions, or even in several
parameters.  This can be used to give a natural type to a function for
computing dot products::

  fun dotProduct(a: [n]i32, b: [n]i32): i32 =
    reduce (+) 0 (zipWith (*) a b)

Or matrix multiplication::

  fun matMult(x: [n][m]i32, y: [m][n]i32): [n][n]i32 =
    ...

The dimension names bound in a parameter shape declaration can be used
as ordinary variables inside the scope of the parameter.

Shape declarations serve two main purposes:

1. They document the shape assumptions of the function in an easily
   understandable form.

2. More importantly, they help the compiler understand the invariants
   of the program, which it may otherwise have trouble figuring out.

Note that adding shape declarations is never unsafe - the compiler
still inserts dynamic checks, so if an incorrect declaration is made,
the result will merely be an abrubt but controlled termination as it
collides with reality.  Shape declarations matter most when used for
the input parameters of the ``main`` function and for the return type
of functions used to ``map``.

User-Defined Operators
~~~~~~~~~~~~~~~~~~~~~~

Infix operators are defined much like functions::

  fun (a:i32,b:i32) +^ (c:i32,d:i32) = (a+c, b+d)

The fixity of an operator is determined by its first characters, which
must correspond to a built-in operator.  Thus, ``+^`` binds like
``+``, whilst ``*^`` binds like ``*``.  The longest such prefix is
used to determine fixity, so ``>>=`` binds like ``>>``, not like
``>``.

It is not permitted to define operators with the names ``&&`` or
``||`` (although these as prefixes are accepted).  This is because a
user-defined version of these operators would not be short-circuiting.
User-defined operators behave exactly like functions, except for
syntactically.

A built-in operator can be shadowed (i.e. a new ``+`` can be defined).
This will result in the built-in polymorphic operator becoming
inaccessible, except through the ``Intrinsics`` module.

.. _entry-points:

Entry Points
~~~~~~~~~~~~

Apart from declaring a function with the keyword ``fun``, it can also
be declared with ``entry``.  When the Futhark program is compiled as a
library instead of an executable program, any function declared with
``entry`` will be exposed as an entry point.

Any function named ``main`` will always be considered an entry point,
whether it is declared with ``entry`` or not.

Value Declarations
------------------

A named value/constant can be declared as follows::

  val name: type = definition

The definition can be an arbitrary expression, including function
calls and other values.  You can even define circular values, although
these will likely result in an infinite loop at execution.  The type
annotation can be elided if the value is defined before it is used.

Values can be used in shape declarations, except in the return value
of entry points.

Type Abbreviations
------------------

Futhark supports simple type abbreviations to improve code readability.
Examples::

  type person_id                = i32
  type int_pair                 = (i32, i32)
  type position, velocity, vec3 = (f32, f32, f32)

  type pilot      = person_id
  type passengers = []person_id
  type mass       = f32

  type airplane = (pilot, passengers, position, velocity, mass)

The abbreviations are merely a syntactic convenience.  With respect to type
checking the ``position`` and ``velocity`` types are identical.  It is
currently not possible to put shape declarations in type abbreviations.
When using uniqueness attributes with type abbreviations, inner uniqueness
attributes are overrided by outer ones::

  type uniqueInts = *[]i32
  type nonuniqueIntLists = []intlist
  type uniqueIntLists = *nonuniqueIntLists

  -- Error: using non-unique value for a unique return value.
  fun uniqueIntLists (nonuniqueIntLists p) = p


Module System
-------------

Futhark supports an ML-style higher-order module system.  *Modules*
can contain types, functions, and other modules.  *Module types* can
be used to classify the contents of modules, and *parameterised
modules* can be used to abstract over modules.  In Standard ML,
modules, module types and parameterised modules are called structs,
signatures, and functors, respectively.

Named module are defined as::

  module ModuleName = module expression

Where a module expression can be the name of another module, an
application of a parameterised module, or a sequence of declarations
enclosed in curly braces::

  module Vec3 = {
    type t = ( f32 , f32 , f32 )
    fun add(a: t) (b: t): t =
      let (a1, a2, a3) = a in
      let (b1, b2, b3) = b in
      (a1 + b1, a2 + b2 , a3 + b3)
  }

  module AlsoVec3 = Vec3

Functions and types within modules can be accessed using dot
notation::

    type vector = Vec3.t
    fun double(v: vector): vector = Vec3.add v v

We can also use ``open Vec3`` to bring the names defined by ``Vec3``
into the current scope.  Multiple modules can be opened simultaneously
by separating their names with spaces.  In case several modules define
the same names, the ones mentioned last take precedence.

Named module types are defined as::

  module type ModuleTypeName = module type expression

A module type expression can be the name of another module type, or a
sequence of *specifications*, or *specs*, enclosed in curly braces.  A
spec can be a *value spec*, indicating the presence of a function or
value, an *abstract type spec*, or a *type abbreviation spec*.  For
example::

  module type Addable = {
    type t                 -- abstract type spec
    type two_ts = (t,t)    -- type abbreviation spec
    val add: t -> t -> t   -- value spec
  }

This module type specifies the presence of an *abstract type* ``t``,
as well as a function operating on values of type ``t``.  We can use
*module type ascription* to restrict a module to what is exposed by
some module type::

  module AbstractVec = Vec3 : Addable

The definition of ``AbstractVec.t`` is now hidden.  In fact, with this
module type, we can neither construct values of type ``AbstractVec.T``
or convert them to anything else, making this a rather useless use of
abstraction.  As a derived form, we can write ``module M: S = e`` to
mean ``module M = e : S``.

Parameterised modules allow us to write definitions that abstract over
modules.  For example::

  module Times(M: Addable) = {
    fun times (x: M.t) (k: int): M.t =
      loop (x' = x) = for i < k do
        T.add x' x
      in x'
  }

We can instantiate ``Times`` with any module that fulfills the module
type ``Addable`` and get back a module that defines a function
``times``::

  module Vec3Times = Times(Vec3)

Now ``Vec3Times.times`` is a function of type ``Vec3.t -> int ->
Vec3.t``.

File Inclusions
---------------

You can include external Futhark code into a Futhark file like this::

  include module

The above will include all functions from whatever ``module`` is and make them
available in the current Futhark program.

All include headers must be at the top of the Futhark file, before any function
declarations.

Currently, Futhark can only include files.  You can include a file into your
main Futhark program like this::

  include other_file

The ``.fut`` extension is implied, so the above will include the file
``other_file.fut``.

You can also include files from subdirectories::

  include path.to.a.file

The above will include the file ``path/to/a/file.fut``.

Simple Expressions
------------------

*constant*
~~~~~~~~~~

Evaluates to itself.

*variable*
~~~~~~~~~~

Evaluates to its value in the environment.

``x`` *arithop* ``y``
~~~~~~~~~~~~~~~~~~~~~

Evaluate the binary arithmetic operator on its operands, which must
both be of the same numeric type.  The following operators are
supported: ``+``, ``*``, ``-``, ``/``, ``%``, ``//``, ``%%``, ``==``,
``!=`` ``<``, ``<=``, ``**``.

``x`` *bitop* ``y``
~~~~~~~~~~~~~~~~~~~

Evaluate the binary bitwise operator on its operands, which must both
be of integer type.  The following operators are supported: ``^``,
``&``, ``|``, ``>>``, ``<<``, ``>>>``, i.e., bitwise xor, and, or,
arithmetic shift right and left, and logical shift right.  Shift
amounts must be non-negative.  Note that, unlike in C, bitwise
operators have *higher* priority than arithmetic operators.  This
means that ``x & y == z`` is understood as ``(x & y) == z``, rather
than ``x & (y == z)`` as it would in C.  Note that the latter is a
type error in Futhark anyhow.

``f x y z``
~~~~~~~~~~~

Apply the function ``f`` to the arguments ``x``, ``y`` and ``z``.

``x && y``
~~~~~~~~~~

Short-circuiting logical conjunction; both operands must be of type
``bool``.

``x || y``
~~~~~~~~~~

Short-circuiting logical disjunction; both operands must be of type ``bool``.

``! x``
~~~~~~~~~

Logical negation of ``x``, which must be of type ``bool``.

``- x``
~~~~~~~

Numerical negation of ``x``, which must be of numeric type.

``~ x``
~~~~~~~

Bitwise negation of ``x``, which must be of integral type.

``abs x``
~~~~~~~~~

Absolute value of ``x``, which must be of integral type.

``signum x``
~~~~~~~~~~~~

Sign of ``x``, which must be of an integral type.  Returns 1, 0, or
-1.

``#i e``
~~~~~~~~

Access field ``i`` of the expression ``e``, which must be of
tuple-type.  The fields are indexed from zero.  ``i`` must be a
literal integer, not an arbitrary expression.

``[x, y, z]``
~~~~~~~~~~~~~

Create an array containing the indicated elements.  Each element must
have the same type and shape.  At least one element must be provided -
empty arrays must be constructed with the ``empty`` construct.

``empty(t)``
~~~~~~~~~~~~

Create an empty array whose row type is ``t``.  For example,
``empty(i32)`` creates a value of type ``[]i32``.  The row type can
contain shape declarations, e.g., ``empty([2]i32)``.  Any dimension
without an annotation will be of size 0, as will the outermost
dimension.


``a[i]``
~~~~~~~~

Return the element at the given position in the array.  The index may
be a comma-separated list of indexes instead of just a single index.
If the number of indices given is less than the rank of the array, an
array is returned.

The array ``a`` must be a variable name or a parenthesized expression.
Futhermore, there *may not* be a space between ``a`` and the opening
bracket.  This disambiguates the array indexing ``a[i]``, from ``a
[i]``, which is a function call with a literal array.

``a[i:j:s]``
~~~~~~~~~~~~

Return a slice of the array ``a`` from index ``i`` to ``j``, the
latter inclusive and the latter exclusive, taking every ``s``th
element.  The ``s`` parameter may not be zero.  If ``s`` is negative,
it means to start at ``i`` and descend by steps of size ``s`` to ``j``
(not inclusive).

It is generally a bad idea for ``s`` to be non-constant.
Slicing of multiple dimensions can be done by separating with commas,
and may be intermixed freely with indexing.

If ``s`` is elided it defaults to ``1``.  If ``i`` or ``j`` is elided,
their value depends on the sign of ``s``.  If ``s`` is positive, ``i``
and ``j`` become ``0`` and the length of the array respectively.  Is
``s`` is negative, ``i`` becomes the length of the array minus one,
and ``j`` becomes minus one.  This means that ``a[::-1]`` is the
reverse of the array ``a``.


``zip x y z``
~~~~~~~~~~~~~~~~~~

Zips together the elements of the outer dimensions of arrays ``x``,
``y``, and ``z``.  Static or runtime check is performed to check that
the sizes of the outermost dimension of the arrays are the same.  If
this property is not true, program execution stops with an error.  Any
number of arrays may be passed to ``unzip``.  If *n* arrays are given,
the result will be a single-dimensional array of *n*-tuples (where the
the tuple components may themselves be arrays).

``zip@i x y z``
~~~~~~~~~~~~~~~~~~

Like ``zip``, but operates within ``i+1`` dimensions.  Thus, ``zip@0``
is equivalent to unadorned ``zip``.  This form is useful when zipping
multidimensional arrays along the innermost dimensions.

``unzip a``
~~~~~~~~~~~~

If the type of ``a`` is ``[(t_1, ..., t_n)]``, the result is a tuple
of *n* arrays, i.e., ``([t_1], ..., [t_n])``, and otherwise a type
error.

``unsafe e``
~~~~~~~~~~~~

Elide safety checks (such as bounds checking) for operations lexically
with ``e``.  This is useful if the compiler is otherwise unable to
avoid bounds checks (e.g. when using indirect indexes), but you really
do not want them here.

``iota n``
~~~~~~~~~~~

An array of the integers from ``0`` to ``n-1``.  The ``n`` argument
can be any integral type.  The elements of the array will have the
same type as ``n``.

``replicate n x``
~~~~~~~~~~~~~~~~~~~

An array consisting of ``n`` copies of ``a``.  The ``n`` argument can
be of any integral type.

``shape a``
~~~~~~~~~~~~~~

The shape of array ``a`` as an integer array.  It is often more
readable to use shape declaration names instead of ``shape``.

``split (i_1, ..., i_n) a``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Partitions the given array ``a`` into ``n+1`` disjoint arrays
``(a[0...i_1-1], a[i_1...i_2-1], ..., a[i_n...])``, returned as a tuple.
The split indices must be weakly ascending, ie ``i_1 <= i_2 <= ... <= i_n``.

Example: ``split((1,1,3), [5,6,7,8]) == ([5],[],[6,7],[8])``

``split@i (i_1, ..., i_n) a``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Splits an array across dimension ``i``, with the outermost dimension
being ``0``.  The ``i`` must be a compile-time integer constant,
i.e. ``i`` cannot be a variable.

``concat a_1 ..., a_n``
~~~~~~~~~~~~~~~~~~~~~~~~~

Concatenate the rows/elements of several arrays.  The shape of the
arrays must be identical in all but the first dimension.  This is
equivalent to ``concat@0`` (see below).

``concat@i a_1 ... a_n``
~~~~~~~~~~~~~~~~~~~~~~~~~

Concatenate arrays across dimension ``i``, with the outermost
dimension being ``0``.  The ``i`` must be a compile-time integer
constant, i.e. ``i`` cannot be a variable.

``copy a``
~~~~~~~~~~~
Return a deep copy of the argument.  Semantically, this is just
the identity function, but it has special semantics related to
uniqueness types as described in :ref:`uniqueness-types`.

``reshape (d_1, ..., d_n) a``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Reshape the elements of ``a`` into an ``n``-dimensional array of the
specified shape.  The number of elements in ``a`` must be equal to the
product of the new dimensions.

``rearrange (d_1, ..., d_n) a``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Permute the dimensions in the array, returning a new array.  The
``d_i`` must be *static* integers, and constitute a proper
length-``n`` permutation.

For example, if ``b==rearrange((2,0,1),a)``, then ``b[x,y,z] =
a[y,z,x]``.


``transpose a``
~~~~~~~~~~~~~~~~

Return the transpose of ``a``, which must be a two-dimensional array.

``rotate@d i a``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Rotate dimension ``d`` of the array ``a`` left by ``i`` elements.
Intuitively, you can think of it as subtracting ``i`` from every index
(modulo the size of the array).

For example, if ``b=rotate(1, i, a)``, then ``b[x,y+1] = a[x,y]``.

``let pat = e in body``
~~~~~~~~~~~~~~~~~~~~~~~

Evaluate ``e`` and bind the result to the pattern ``pat`` while
evaluating ``body``.  The ``in`` keyword is optional if ``body`` is a
``let`` or ``loop`` expression.

``let a[i] = v in body``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Write ``v`` to ``a[i]`` and evaluate ``body``.  The given index need
not be complete and can also be a slice, but in these cases, the value
of ``v`` must be an array of the proper size.

``if c then a else b``
~~~~~~~~~~~~~~~~~~~~~~

If ``c`` evaluates to ``True``, evaluate ``a``, else evaluate ``b``.

``loop (pat = initial) = for i < bound do loopbody in body``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The name ``i`` is bound here and initialised to zero.

1. Bind ``pat`` to the initial values given in ``initial``.

2. While ``i < bound``, evaluate ``loopbody``, rebinding ``pat`` to be
   the value returned by the body, increasing ``i`` by one after each
   iteration.

3. Evaluate ``body`` with ``pat`` bound to its final
   value.

The ``= initial`` can be left out, in which case initial values for
the pattern are taken from equivalently named variables in the
environment.  I.e., ``loop (x) = ...`` is equivalent to ``loop (x = x)
= ...``.

``loop (pat = initial) = while cond do loopbody in body``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. Bind ``pat`` to the initial values given in ``initial``.

2. While ``cond`` evaluates to true, evaluate ``loopbody``, rebinding
   ``pat`` to be the value returned by the body.

3. Evaluate ``body`` with ``pat`` bound to its final value.

Parallel Expressions
--------------------

It is not guaranteed that the the parallel constructs in Futhark are
evaluated in parallel, especially if they are nested in complicated
ways.  Their purpose is to give the compiler as much freedom and
information is possible, in order to enable it to maximise the
parallelism of the generated code.

``map f a_1 ... a_n``
~~~~~~~~~~~~~~~~~~~~~

Apply ``f`` to every element of ``a_1 ... a_n`` and return the
resulting array.  Differs from ``map f (zip a_1 ... a_n)`` in that
``f`` is called with ``n`` arguments, where in the latter case it is
called with a single ``n``-tuple argument.  In other languages, this
form of ``map`` is often called ``zipWith``.

``zipWith f a_1 ... a_n``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Alias for ``map`` for backwards compatibility.

``reduce f x a``
~~~~~~~~~~~~~~~~~~~

Left-reduction with ``f`` across the elements of ``a``, with ``x`` as
the neutral element for ``f``.  The function ``f`` must be
associative.  If it is not, the return value is unspecified.

``reduceComm f x a``
~~~~~~~~~~~~~~~~~~~~

Like ``reduce``, but with the added guarantee that the function ``f``
is *commutative*.  This lets the compiler generate more efficient
code.  If ``f`` is not commutative, the return value is unspecified.
You do not need to explicitly use ``reduceComm`` with built-in
operators like ``+`` - the compiler already knows that these are
commutative.

``scan f x a``
~~~~~~~~~~~~~~~~~~~

Inclusive prefix scan.  Has the same caveats with respect to
associativity as ``reduce``.

``filter f a``
~~~~~~~~~~~~~~~~

Remove all those elements of ``a`` that do not satisfy the predicate
``f``.

``partition (f_1, ..., f_n) a``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Divide the array ``a`` into disjoint partitions based on the given
predicates.  Each element of ``a`` is called with the predicates
``f_1`` to ``f_n`` in sequence, and as soon as one as one of them
returns ``True``, the element is added to the corresponding partition.
If none of the functions return ``True``, the element is added to a
catch-all partition that is returned last.  Always returns a tuple
with *n+1* components.  The partitioning is stable, meaning that
elements of the partitions retain their original relative positions.

``write is vs as``
~~~~~~~~~~~~~~~~~~

The ``write`` expression calculates the equivalent of this imperative
code::

  for index in 0..shape(is)[0]-1:
    i = is[index]
    v = vs[index]
    as[i] = v

The ``is`` and ``vs`` arrays must have the same outer size.  ``write``
acts in-place and consumes the ``as`` array, returning a new array
that has the same type and elements as ``as``, except for the indices
in ``is``.  If ``is`` contains duplicates (i.e. several writes are
performed to the same location), the result is unspecified.  It is not
guaranteed that one of the duplicate writes will complete atomically -
they may be interleaved.

Arrays of Tuples
----------------

For reasons related to code generation and efficient representation,
arrays of tuples are in a sense merely syntactic sugar for tuples of
arrays.  The type ``[](i32,f32)`` is transformed to ``([]i32,
[]f32)`` during the compilation process, and all code interacting
with arrays of tuples is likewise transformed.  In most cases, this is
fully transparent to the programmer, but there are edge cases where
the transformation is not trivially an isomorphism.

Consider the type ``[]([]i32,[]f32)``, which is transformed
into ``([][]i32, [][]f32)``.  These two types are not
isomorphic, as the latter has more stringent demands as to the
fullness of arrays.  For example::

  [
    ([1],   [1.0]),
    ([2,3], [2.0])
  ]

is a value of the former, but the first element of the
corresponding transformed tuple::

  (
    [[1],   [2, 3]],
    [[1.0], [2.0]]
  )

is not a full array.  Hence, when determining whether a program
generates full arrays, we must hence look at the *transformed*
values - in a sense, the fullness requirement "transcends" the tuples.

Another, less operational, way of understanding the problem is to look
at the type of an array of a pair of arrays::

  ``[]([]t1, []t2)``

For this to be a valid type, we must be able to insert shape
declarations::

  ``[n]([m1]t1, [m2]t2)``

This require that the inner arrays have size ``m1`` and ``m2``
respectively (without specifying exactly what that is).  An irregular
array value could not possibly have this type for constant ``m1``,
``m2``.

Literal Defaults
----------------

By default, Futhark interprets integer literals as ``i32`` values, and decimal
literals (integer literals containing a decimal point) as ``f64`` values. These
defaults can be changed using the `Haskell-inspired
<https://wiki.haskell.org/Keywords#default>`_ ``default`` keyword.

To change the ``i32`` default to e.g. ``i64``, type the following at the top of
your file::

  default(i64)

To change the ``f64`` default to ``f32``, type the following at the top of your
file::

  default(f32)

To change both, type::

  default(i64,f32)
