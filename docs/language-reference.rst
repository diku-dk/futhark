.. _language-reference:

Language Reference
==================

This reference seeks to describe every construct in the Futhark
language.  It is not presented in a tutorial fashion, but rather
intended for quick lookup and documentation of subtleties.  For this
reason, it is not written in a bottom-up manner, and some concepts may
be used before they are fully defined.  It is a good idea to have a
basic grasp of Futhark (or some other functional programming language)
before reading this reference.  An ambiguous grammar is given for the
full language.  The text describes how ambiguities are resolved in
practice (things like operator precedence).

Identifiers and Keywords
------------------------

.. productionlist::
   id: `letter` (`letter` | "_" | "'")* | "_" `id`
   quals: (`id` ".")+
   qualid: `id` | `quals` `id`
   binop: `opstartchar` `opchar`*
   qualbinop: `binop` | `quals` `binop`
   fieldid: `decimal` | `id`
   opstartchar = "+" | "-" | "*" | "/" | "%" | "=" | "!" | ">" | "<" | "|" | "&" | "^"
   opchar: `opstartchar` | "."

Many things in Futhark are named. When we are defining something, we
give it an unqualified name (`id`).  When referencing something inside
a module, we use a qualified name (`qualid`).  The fields of a record
are named with `fieldid`.  Note that a `fieldid` can be a decimal
number.

Primitive Types and Values
--------------------------

.. productionlist::
   literal: `intnumber` | `floatnumber` | "true" | "false"

Boolean literals are written ``true`` and ``false``.  The primitive
types in Futhark are the signed integer types ``i8``, ``i16``,
``i32``, ``i64``, the unsigned integer types ``u8``, ``u16``, ``u32``,
``u64``, the floating-point types ``f32``, ``f64``, as well as
``bool``.  An ``f32`` is always a single-precision float and a ``f64``
is a double-precision float.

.. productionlist::
   int_type: "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64"
   float_type: "f8" | "f16" | "f32" | "f64"

Numeric literals can be suffixed with their intended type.  For
example ``42i8`` is of type ``i8``, and ``1337e2f64`` is of type
``f64``.  If no suffix is given, integer literals are of type ``i32``,
and decimal literals are of type ``f64``.  Hexadecimal literals are
supported by prefixing with ``0x``, and binary literals by prefixing
with ``0b``.

.. productionlist::
   intnumber: (`decimal` | `hexadecimal` | `binary`) [`int_type`]
   decimal: `decdigit` (`decdigit` |"_")*
   hexadecimal: 0 ("x" | "X") `hexdigit` (`hexdigit` |"_")*
   binary: 0 ("b" | "B") `bindigit` (`bindigit` | "_")*

.. productionlist::
   floatnumber: (`pointfloat` | `exponentfloat`) [`float_type`]
   pointfloat: [`intpart`] `fraction`
   exponentfloat: (`intpart` | `pointfloat`) `exponent`
   intpart: `decdigit` (`decdigit` |"_")*
   fraction: "." `decdigit` (`decdigit` |"_")*
   exponent: ("e" | "E") ["+" | "-"] `decdigit`+

.. productionlist::
   decdigit: "0"..."9"
   hexdigit: `decdigit` | "a"..."f" | "A"..."F"
   bindigit: "0" | "1"

Numeric values can be converted between different types by using the
desired type name as a function.  E.g., ``i32(1.0f32)`` would convert
the floating-point number ``1.0`` to a 32-bit signed integer.
Conversion from floating-point to integers is done by truncation.

Boolean values can also be converted to numbers (1 for true, 0 for
false) by using the desired numeric type as a function.

Compound Types and Values
~~~~~~~~~~~~~~~~~~~~~~~~~

All primitive values can be combined in tuples and arrays.  A tuple
value or type is written as a sequence of comma-separated values or
types enclosed in parentheses.  For example, ``(0, 1)`` is a tuple
value of type ``(i32,i32)``.  The elements of a tuple need not have
the same type -- the value ``(false, 1, 2.0)`` is of type ``(bool,
i32, f64)``.  A tuple element can also be another tuple, as in
``((1,2),(3,4))``, which is of type ``((i32,i32),(i32,i32))``.  A
tuple cannot have just one element, but empty tuples are permitted,
although they are not very useful-these are written ``()`` and are of
type ``()``.

.. productionlist::
   type: `qualid` | `array_type` | `tuple_type` | `record_type` | `type` `type_arg`
   array_type: "[" [`dim`] "]" `type`
   tuple_type: "(" ")" | "(" `type` ("[" "," `type` "]")* ")"
   record_type: "{" "}" | "{" `fieldid` ":" `type` ("," `fieldid` ":" `type`)* "}"
   type_arg: "[" [`dim`] "]" | `type`
   dim: `qualid` | `decimal` | "#" `id`

An array value is written as a nonempty sequence of comma-separated
values enclosed in square brackets: ``[1,2,3]``.  An array type is
written as ``[d]t``, where ``t`` is the element type of the array, and
``d`` is an integer indicating the size.  We typically elide ``d``, in
which case the size will be inferred.  As an example, an array of
three integers could be written as ``[1,2,3]``, and has type
``[3]i32``.  An empty array is written as ``empty(t)``, where ``t`` is
the element type.

Multi-dimensional arrays are supported in Futhark, but they must be
*regular*, meaning that all inner arrays must have the same shape.
For example, ``[[1,2], [3,4], [5,6]]`` is a valid array of type
``[3][2]i32``, but ``[[1,2], [3,4,5], [6,7]]`` is not, because there
we cannot come up with integers ``m`` and ``n`` such that
``[m][n]i32`` describes the array.  The restriction to regular arrays
is rooted in low-level concerns about efficient compilation.  However,
we can understand it in language terms by the inability to write a
type with consistent dimension sizes for an irregular array value.  In
a Futhark program, all array values, including intermediate (unnamed)
arrays, must be typeable.

Records are mappings from field names to values, with the field names
known statically.  A tuple behaves in all respects like a record with
numeric field names, and vice versa.  It is an error for a record type
to name the same field twice.

A parametric type abbreviation can be applied by juxtaposing its name
and its arguments.  The application must provide as many arguments as
the type abbreviation has parameters - partial application is
presently not allowed.  See `Type Abbreviations`_ for further details.

String literals are supported, but only as syntactic sugar for arrays
of ``i32`` values.  There is no ``char`` type in Futhark.

.. productionlist::
   stringlit: '"' `stringchar` '"'
   stringchar: <any source character except "\" or newline or quotes>

Expressions
-----------

Expressions are the basic construct of any Futhark program.  An
expression has a statically determined *type*, and produces a *value*
at runtime.  Futhark is an eager/strict language ("call by value").

The basic elements of expressions are called *atoms*, for example
literals and variables, but also more complicated forms.

.. productionlist::
   atom:   `literal`
       : | `qualid`
       : | `stringlit`
       : | "empty" "(" `type` ")"
       : | "(" ")"
       : | "(" `exp` ")"
       : | "(" `exp` ("," `exp`)* ")"
       : | "{" "}"
       : | "{" field ("," `field`)* "}"
       : | `qualid` "[" `index` ("," `index`)* "]"
       : | "(" `exp` ")" "[" `index` ("," `index`)* "]"
       : | "#" `fieldid` `exp`
       : | `quals`."(" `exp` ")"
       : | "[" `exp` ("," `exp`)* "]"
       : | "[" `exp` [".." `exp`] "..." `exp` "]"
   exp:   `atom`
      : | `exp` `qualbinop` `exp`
      : | `exp` `exp`
      : | `exp` ":" `type`
      : | "if" `exp` "then" `exp` "else" `exp`
      : | "let" `type_param`* `pat` "=" `exp` "in" `exp`
      : | "let" `id` "[" `index` ("," `index`)* "]" "=" `exp` "in" `exp`
      : | "let" `id` `type_param`* `pat`+ [":" `type`] "=" `exp` "in" `exp`
      : | "loop" `type_param`* `pat` [("=" `exp`)] `loopform` "do" `exp`
      : | "reshape" `exp` `exp`
      : | "rearrange" "(" `nat_int`+ ")" `exp`
      : | "rotate" ["@" `nat_int`] `exp` `exp`
      : | "split" ["@" `nat_int`] `exp` `exp`
      : | "concat" ["@" `nat_int`] `exp`+
      : | "zip" ["@" `nat_int`] `exp`+
      : | "unzip" `exp`
      : | "unsafe" `exp`
      : | `exp` "with" "[" `index` ("," `index`)* "]" "<-" `exp`
      : | "map" `fun` `exp`+
      : | "reduce" `fun` `exp` `exp`
      : | "reduce_comm" `fun` `exp` `exp`
      : | "reduce" `fun` `exp` `exp`
      : | "scan" `fun` `exp` `exp`
      : | "filter" `fun` `exp`
      : | "partition" "(" `fun`+ ")" `exp`
      : | "stream_map" `fun` `exp`
      : | "stream_map_per" `fun` `exp`
      : | "stream_red" `fun` `exp` `exp`
      : | "stream_map_per" `fun` `exp` `exp`
      : | "stream_seq" `fun` `exp` `exp`
   field:   `fieldid` "=" `exp`
        : | `exp`
   pat:   `id`
      : |  "_"
      : | "(" ")"
      : | "(" `pat` ")"
      : | "(" `pat` ("," `pat`)+ ")"
      : | "{" "}"
      : | "{" `fieldid` "=" `pat` ["," `fieldid` "=" `pat`] "}"
      : | `pat` ":" `type`
   loopform :   "for" `id` "<" `exp`
            : | "for" `pat` "in" `exp`
            : | "while" `exp`

Some of the built-in expression forms have parallel semantics, but it
is not guaranteed that the the parallel constructs in Futhark are
evaluated in parallel, especially if they are nested in complicated
ways.  Their purpose is to give the compiler as much freedom and
information is possible, in order to enable it to maximise the
efficiency of the generated code.

Resolving Ambiguities
~~~~~~~~~~~~~~~~~~~~~

The above grammar contains some ambiguities, which in the concrete
implementation is resolved via a combination of lexer and grammar
transformations.  For ease of understanding, they are presented here
in natural text.

* A type ascription (`exp` ``:`` `type`) cannot appear as an array
  index, as it collides with the syntax for slicing.

* In ``f [x]``, there is am ambiguity between indexing the array ``f``
  at position ``x``, or calling the function ``f`` with the singleton
  array ``x``.  We resolve this the following way:

    * If there is a space between ``f`` and the opening bracket, it is
      treated as a function application.

    * Otherwise, it is an array index operation.

* The following table describes the precedence and associativity of
  infix operators.  All operators in the same row have the same
  precedence.  The rows are listed in increasing order of precedence.
  Note that not all operators listed here are used in expressions;
  nevertheless, they are still used for resolving ambiguities.

  =================  =============
  **Associativity**  **Operators**
  =================  =============
  left               ``,``
  left               ``:``
  left               ``||``
  left               ``&&``
  left               ``<=`` ``>=`` ``>`` ``<`` ``==`` ``!=``
  left               ``&`` ``^`` ``|``
  left               ``<<`` ``>>`` ``>>>``
  left               ``+`` ``-``
  left               ``*`` ``/`` ``%`` ``//`` ``%%``
  right              ``->``
  =================  =============

Semantics
~~~~~~~~~

`literal`
.........

Evaluates to itself.

`qualid`
........

A variable name; evaluates to its value in the current environment.

`stringlit`
...........

Evaluates to an array of type ``[]i32`` that contains the string
characters as integers.

``empty(t)``
............

Create an empty array whose row type is ``t``.  For example,
``empty(i32)`` creates a value of type ``[]i32``.  The row type can
contain shape declarations, e.g., ``empty([2]i32)``.  Any dimension
without an annotation will be of size 0, as will the outermost
dimension.

``()``
......

Evaluates to an empty tuple.

``( e )``
.........

Evaluates to the result of ``e``.

``(e1, e2, ..., eN)``
.....................

Evaluates to a tuple containing ``N`` values.  Equivalent to ``(1=e1,
2=e2, ..., N=eN)``.

``{f1, f2, ..., fN}``
.....................

A record expression consists of a comma-separated sequence of *field
expressions*.  A record expression is evaluated by creating an empty
record, then processing the field expressions from left to right.
Each field expression adds fields to the record being constructed.  A
field expression can take one of two forms:

  ``f = e``: adds a field with the name ``f`` and the value resulting
  from evaluating ``e``.

  ``e``: the expression ``e`` must evaluate to a record, whose fields
  are added to the record being constructed.

If a field expression attempts to add a field that already exists in
the record being constructed, the new value for the field supercedes
the old one.

``a[i]``
........

Return the element at the given position in the array.  The index may
be a comma-separated list of indexes instead of just a single index.
If the number of indices given is less than the rank of the array, an
array is returned.

The array ``a`` must be a variable name or a parenthesized expression.
Futhermore, there *may not* be a space between ``a`` and the opening
bracket.  This disambiguates the array indexing ``a[i]``, from ``a
[i]``, which is a function call with a literal array.

``a[i:j:s]``
............

Return a slice of the array ``a`` from index ``i`` to ``j``, the
latter inclusive and the latter exclusive, taking every ``s``-th
element.  The ``s`` parameter may not be zero.  If ``s`` is negative,
it means to start at ``i`` and descend by steps of size ``s`` to ``j``
(not inclusive).

It is generally a bad idea for ``s`` to be non-constant.
Slicing of multiple dimensions can be done by separating with commas,
and may be intermixed freely with indexing.

If ``s`` is elided it defaults to ``1``.  If ``i`` or ``j`` is elided, their
value depends on the sign of ``s``.  If ``s`` is positive, ``i`` become ``0``
and ``j`` become the length of the array.  If ``s`` is negative, ``i`` becomes
the length of the array minus one, and ``j`` becomes minus one.  This means that
``a[::-1]`` is the reverse of the array ``a``.

``[x, y, z]``
.............

Create an array containing the indicated elements.  Each element must
have the same type and shape.  At least one element must be provided -
empty arrays must be constructed with the ``empty`` construct.  This
restriction is due to limited type inference in the Futhark compiler,
and will hopefully be fixed in the future.

``[x..y...z]``
..............

Construct an integer array whose first element is ``x`` and which
proceeds stride of ``y-x`` until reaching ``z`` (inclusive).  The
``..y`` part can be elided in which case a stride of 1 is used.  The
stride may not be zero.  An empty array is returned in cases where
``z`` would never be reached or ``x`` and ``y`` are the same value.

``[x..y..<z]``
...............

Construct an integer array whose first elements is ``x``, and which
proceeds upwards with a stride of ``y`` until reaching ``z``
(exclusive).  The ``..y`` part can be elided in which case a stride of
1 is used.  An empty array is returned in cases where ``z`` would
never be reached or ``x`` and ``y`` are the same value.

``[x..y..>z]``
...............

Construct an integer array whose first elements is ``x``, and which
proceeds downwards with a stride of ``y`` until reaching ``z``
(exclusive).  The ``..y`` part can be elided in which case a stride of
-1 is used.  An empty array is returned in cases where ``z`` would
never be reached or ``x`` and ``y`` are the same value.

``#f e``
........

Access field ``f`` of the expression ``e``, which must be a record or
tuple.

``m.(e)``
.........

Evaluate the expression ``e`` with the module ``m`` locally opened, as
if by ``open``.  This can make some expressions easier to read and
write, without polluting the global scope with a declaration-level
``open``.

``x`` *binop* ``y``
...................

Apply an operator to ``x`` and ``y``.  Operators are functions like
any other, and can be user-defined.  Futhark pre-defines certain
"magical" *overloaded* operators that work on many different types.
Overloaded functions cannot be defined by the user.  Both operands
must have the same type.  The predefined operators and their semantics
are:

  ``**``

    Power operator, defined for all numeric types.

  ``//``, ``%%``

    Division and remainder on integers, with rounding towards zero.

  ``*``, ``/``, ``%``, ``+``, ``-``

    The usual arithmetic operators, defined for all numeric types.
    Note that ``/`` and ``%`` rounds towards negative infinity when
    used on integers - this is different from in C.

  ``^``, ``&``, ``|``, ``>>``, ``<<``, ``>>>``

    Bitwise operators, respectively bitwise xor, and, or, arithmetic
    shift right and left, and logical shift right.  Shift amounts
    must be non-negative and the operands must be integers.  Note
    that, unlike in C, bitwise operators have *higher* priority than
    arithmetic operators.  This means that ``x & y == z`` is
    understood as ``(x & y) == z``, rather than ``x & (y == z)`` as
    it would in C.  Note that the latter is a type error in Futhark
    anyhow.

  ``==``, ``!=``

      Compare any two values of builtin or compound type for equality.

  ``<``, ``<=``.  ``>``, ``>=``

      Company any two values of numeric type for equality.

``x && y``
..........

Short-circuiting logical conjunction; both operands must be of type
``bool``.

``x || y``
..........

Short-circuiting logical disjunction; both operands must be of type
``bool``.

``f x y z``
...........

Apply the function ``f`` to the arguments ``x``, ``y`` and ``z``.  Any
number of arguments can be passed.

``e : t``
.........

Annotate that ``e`` is expected to be of type ``t``, failing with a
type error if it is not.  If ``t`` is an array with shape
declarations, the correctness of the shape declarations is checked at
run-time.

Due to ambiguities, this syntactic form cannot appear as an array
index expression unless it is first enclosed in parentheses.

``! x``
.........

Logical negation of ``x``, which must be of type ``bool``.

``- x``
.......

Numerical negation of ``x``, which must be of numeric type.

``. x``
.......

Bitwise negation of ``x``, which must be of integral type.

``if c then a else b``
......................

If ``c`` evaluates to ``True``, evaluate ``a``, else evaluate ``b``.

``let pat = e in body``
.......................

Evaluate ``e`` and bind the result to the pattern ``pat`` while
evaluating ``body``.  The ``in`` keyword is optional if ``body`` is a
``let`` expression. See also `Shape Declarations`_.

``let a[i] = v in body``
........................................

Write ``v`` to ``a[i]`` and evaluate ``body``.  The given index need
not be complete and can also be a slice, but in these cases, the value
of ``v`` must be an array of the proper size.  Syntactic sugar for
``let a = a with [i] <- v in a``.

``let f params... = e in body``
...............................

Bind ``f`` to a function with the given parameters and definition
(``e``) and evaluate ``body``.  The function will be treated as
aliasing any free variables in ``e``.  The function is not in scope of
itself, and hence cannot be recursive.  See also `Shape
Declarations`_.

``loop pat = initial for x in a do loopbody``
.............................................

1. Bind ``pat`` to the initial values given in ``initial``.

2. For each element ``x`` in ``a``, evaluate ``loopbody`` and rebind
   ``pat`` to the result of the evaluation.

3. Return the final value of ``pat``.

The ``= initial`` can be left out, in which case initial values for
the pattern are taken from equivalently named variables in the
environment.  I.e., ``loop (x) = ...`` is equivalent to ``loop (x = x)
= ...``.

See also `Shape Declarations`_.

``loop pat = initial for x < n do loopbody``
............................................

Equivalent to ``loop (pat = initial) for x in iota n do loopbody``.

``loop pat = initial = while cond do loopbody``
...............................................

1. Bind ``pat`` to the initial values given in ``initial``.

2. If ``cond`` evaluates to true, bind ``pat`` to the result of
   evaluating ``loopbody``, and repeat the step.

3. Return the final value of ``pat``.

See also `Shape Declarations`_.

``iota n``
...........

An array of the integers from ``0`` to ``n-1``.  The ``n`` argument
can be any integral type.  The elements of the array will have the
same type as ``n``.

``shape a``
..............

The shape of array ``a`` as an integer array.  It is often more
readable to use shape declaration names instead of ``shape``.

``replicate n x``
...................

An array consisting of ``n`` copies of ``a``.  The ``n`` argument must
be of type ``i32``.

``reshape (d_1, ..., d_n) a``
...............................

Reshape the elements of ``a`` into an ``n``-dimensional array of the
specified shape.  The number of elements in ``a`` must be equal to the
product of the new dimensions.

``rearrange (d_1, ..., d_n) a``
..................................

Permute the dimensions in the array, returning a new array.  The
``d_i`` must be *static* integers, and constitute a proper
length-``n`` permutation.

For example, if ``b==rearrange (2,0,1) a``, then ``b[x,y,z] =
a[y,z,x]``.

``rotate@d i a``
................

Rotate dimension ``d`` of the array ``a`` left by ``i`` elements.
Intuitively, you can think of it as subtracting ``i`` from every index
(modulo the size of the array).

For example, if ``b==rotate 1 i a``, then ``b[x,y+1] = a[x,y]``.

``split (i_1, ..., i_n) a``
.............................

Partitions the given array ``a`` into ``n+1`` disjoint arrays
``(a[0...i_1-1], a[i_1...i_2-1], ..., a[i_n...])``, returned as a tuple.
The split indices must be weakly ascending, ie ``i_1 <= i_2 <= ... <= i_n``.

Example: ``split (1,1,3) [5,6,7,8] == ([5],[],[6,7],[8])``

``split@i (i_1, ..., i_n) a``
.............................

Splits an array across dimension ``i``, with the outermost dimension
being ``0``.  The ``i`` must be a compile-time integer constant,
i.e. ``i`` cannot be a variable.

``concat a_1 ..., a_n``
.........................

Concatenate the rows/elements of several arrays.  The shape of the
arrays must be identical in all but the first dimension.  This is
equivalent to ``concat@0`` (see below).

``concat@i a_1 ... a_n``
.........................

Concatenate arrays across dimension ``i``, with the outermost
dimension being ``0``.  The ``i`` must be a compile-time integer
constant, i.e. ``i`` cannot be a variable.

``zip x y z``
..................

Zips together the elements of the outer dimensions of arrays ``x``,
``y``, and ``z``.  Static or runtime check is performed to check that
the sizes of the outermost dimension of the arrays are the same.  If
this property is not true, program execution stops with an error.  Any
number of arrays may be passed to ``unzip``.  If *n* arrays are given,
the result will be a single-dimensional array of *n*-tuples (where the
the tuple components may themselves be arrays).

``zip@i x y z``
..................

Like ``zip``, but operates within ``i+1`` dimensions.  Thus, ``zip@0``
is equivalent to unadorned ``zip``.  This form is useful when zipping
multidimensional arrays along the innermost dimensions.

``unzip a``
............

If the type of ``a`` is ``[(t_1, ..., t_n)]``, the result is a tuple
of *n* arrays, i.e., ``([t_1], ..., [t_n])``, and otherwise a type
error.

``unsafe e``
............

Elide safety checks (such as bounds checking) for operations lexically
with ``e``.  This is useful if the compiler is otherwise unable to
avoid bounds checks (e.g. when using indirect indexes), but you really
do not want them here.

``a with [i] <- e``
...................

Return ``a``, but with the element at position ``i`` changed to
contain the result of evaluating ``e``.  Consumes ``a``.

``map f a_1 ... a_n``
.....................

Apply ``f`` to every element of ``a_1 ... a_n`` and return the
resulting array.  Differs from ``map f (zip a_1 ... a_n)`` in that
``f`` is called with ``n`` arguments, where in the latter case it is
called with a single ``n``-tuple argument.  In other languages, this
form of ``map`` is often called ``zipWith``.

``reduce f x a``
...................

Left-reduction with ``f`` across the elements of ``a``, with ``x`` as
the neutral element for ``f``.  The function ``f`` must be
associative.  If it is not, the return value is unspecified.

``reduce_comm f x a``
.....................

Like ``reduce``, but with the added guarantee that the function ``f``
is *commutative*.  This lets the compiler generate more efficient
code.  If ``f`` is not commutative, the return value is unspecified.
You do not need to explicitly use ``reduce_comm`` with built-in
operators like ``+`` - the compiler already knows that these are
commutative.

``scan f x a``
...................

Inclusive prefix scan.  Has the same caveats with respect to
associativity as ``reduce``.

``filter f a``
................

Remove all those elements of ``a`` that do not satisfy the predicate
``f``.

``partition (f_1, ..., f_n) a``
...............................

Divide the array ``a`` into disjoint partitions based on the given
predicates.  Each element of ``a`` is called with the predicates
``f_1`` to ``f_n`` in sequence, and as soon as one as one of them
returns ``True``, the element is added to the corresponding partition.
If none of the functions return ``True``, the element is added to a
catch-all partition that is returned last.  Always returns a tuple
with *n+1* components.  The partitioning is stable, meaning that
elements of the partitions retain their original relative positions.

``scatter as is vs``
....................

This ``scatter`` expression calculates the equivalent of this imperative
code::

  for index in 0..shape(is)[0]-1:
    i = is[index]
    v = vs[index]
    as[i] = v

The ``is`` and ``vs`` arrays must have the same outer size.  ``scatter``
acts in-place and consumes the ``as`` array, returning a new array
that has the same type and elements as ``as``, except for the indices
in ``is``.  If ``is`` contains duplicates (i.e. several writes are
performed to the same location), the result is unspecified.  It is not
guaranteed that one of the duplicate writes will complete atomically -
they may be interleaved.

Shape Declarations
------------------

Whenever a pattern occurs (in ``let``, ``loop``, and function
parameters), as well as in return types, *shape declarations* may be
used to express invariants about the shapes of arrays
that are accepted or produced by the function.  For example::

  let f (a: [#n]i32) (b: [#n]i32): [n]i32 =
    map (+) a b

When prefixed with a ``#`` character, a name is *freshly bound*,
whilst an unadorned name must be in scope.  In the example above,
``#`` is not used in the return type, because we wish to refer to the
``n`` bound by the parameters.  If we refer to the same freshly bound
variable in multiple parameters (as above), each occurence must be
prefixed with ``#``.  The sizes can also be explicitly quantified::

  let f [n] (a: [n]i32) (b: [n]i32): [n]i32 =
    map (+) a b

This has the same meaning as above.  It is an error to mix explicit
and implicit sizes.  Note that the ``[n]`` parameter need not be
explicitly passed when calling ``f``.  Any explicitly bound size must
be used in a parameters.  This is an error::

  let f [n] (x: i32) = n

A shape declaration can also be an integer constant (with no suffix).
The dimension names bound can be used as ordinary variables within the
scope of the parameters.  If a function is called with arguments, or
returns a value, that does not fulfill the shape constraints, the
program will fail with a runtime error.  Likewise, if a pattern with
shape declarations is attempted bound to a value that does not fulfill
the invariants, the program will fail with a runtime error.  For
example, this will fail::

  let x: [3]i32 = iota 2

While this will succeed and bind ``n`` to ``2``::

  let [n] x: [n]i32 = iota 2

Declarations
------------

.. productionlist::
   dec:   `fun_bind` | `val_bind` | `type_bind` | `mod_bind` | `mod_type_bind`
      : | "open" `mod_exp`+
      : | `default_dec`
      : | "import" `stringlit`

Declaring Functions and Values
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. productionlist::
   fun_bind:   ("let" | "entry") `id` `type_param`* `pat`+ [":" `type`] "=" `exp`
           : | ("let" | "entry") `pat` `binop` `pat` [":" `type`] "=" `exp`

.. productionlist::
   val_bind: "let" `id` [":" `type`] "=" `exp`

Functions and values must be defined before they are used.  A function
declaration must specify the name, parameters, return type, and body
of the function::

  let name params...: rettype = body

Type inference is not supported, and functions are fully monomorphic.
A parameter is written as ``(name: type)``.  Functions may not be
recursive.  Optionally, the programmer may put *shape declarations* in
the return type and parameter types; see `Shape Declarations`_.  A
function can be *polymorphic* by using type parameters, in the same
way as for `Type Abbreviations`_::

  let reverse [n] 't (xs: [n]t): [n]t = xs[::-1]

Shape and type parameters are not passed explicitly when calling
function, but are automatically derived.

User-Defined Operators
~~~~~~~~~~~~~~~~~~~~~~

Infix operators are defined much like functions::

  let (p1: t1) op (p2: t2): rt = ...

For example::

  let (a:i32,b:i32) +^ (c:i32,d:i32) = (a+c, b+d)

A valid operator name is a non-empty sequence of characters chosen
from the string ``"+-*/%=!><&^"``.  The fixity of an operator is
determined by its first characters, which must correspond to a
built-in operator.  Thus, ``+^`` binds like ``+``, whilst ``*^`` binds
like ``*``.  The longest such prefix is used to determine fixity, so
``>>=`` binds like ``>>``, not like ``>``.

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
............

Apart from declaring a function with the keyword ``fun``, it can also
be declared with ``entry``.  When the Futhark program is compiled any
function declared with ``entry`` will be exposed as an entry point.
If the Futhark program has been compiled as a library, these are the
functions that will be exposed.  If compiled as an executable, you can
use the ``--entry-point`` command line option of the generated
executable to select the entry point you wish to run.

Any function named ``main`` will always be considered an entry point,
whether it is declared with ``entry`` or not.

Value Declarations
..................

A named value/constant can be declared as follows::

  let name: type = definition

The definition can be an arbitrary expression, including function
calls and other values, although they must be in scope before the
value is defined.  The type annotation can be elided if the value is
defined before it is used.

Values can be used in shape declarations, except in the return value
of entry points.

Type Abbreviations
~~~~~~~~~~~~~~~~~~

.. productionlist::
   type_bind: "type" `id` `type_param`* "=" `type`
   type_param: "[" `id` "]" | "'" `id`

Type abbreviations function as shorthands for purpose of documentation
or brevity.  After a type binding ``type t1 = t2``, the name ``t1``
can be used as a shorthand for the type ``t2``.  Type abbreviations do
not create new unique types.  After the previous binding, the types
``t1`` and ``t2`` are entirely interchangeable.

A type abbreviation can have zero or more parameters.  A type
parameter enclosed with square brackets is a *shape parameter*, and
can be used in the definition as an array dimension size, or as a
dimension argument to other type abbreviations.  When passing an
argument for a shape parameter, it must be encloses in square
brackets.  Example::

  type two_intvecs [n] = ([n]i32, [n]i32)

  let (a,b): two_intvecs [2] = (iota 2, replicate 2 0)

Shape parameters work much like shape declarations for arrays.  Like
shape declarations, they can be elided via square brackets containing
nothing.

A type parameter prefixed with a single quote is a *type parameter*.
It is in scope as a type in the definition of the type abbreviation.
Whenever the type abbreviation is used in a type expression, a type
argument must be passed for the parameter.  Type arguments need not be
prefixed with single quotes::

  type two_vecs [n] 't = ([n]t, [n]t)
  type two_intvecs [n] = two_vecs [n] i32
  let (a,b): two_vecs [2] i32 = (iota 2, replicate 2 0)

When using uniqueness attributes with type abbreviations, inner
uniqueness attributes are overrided by outer ones::

  type unique_ints = *[]i32
  type nonunique_int_lists = []unique_ints
  type unique_int_lists = *nonunique_int_lists

  -- Error: using non-unique value for a unique return value.
  let f (p: nonunique_int_lists): unique_int_lists = p


Module System
-------------

.. productionlist::
   mod_bind: "module" `id` `mod_param`+ "=" [":" mod_type_exp] "=" `mod_exp`
   mod_param: "(" `id` ":" `mod_type_exp` ")"
   mod_type_bind: "module" "type" `id` `type_param`* "=" `mod_type_exp`

Futhark supports an ML-style higher-order module system.  *Modules*
can contain types, functions, and other modules.  *Module types* are
used to classify the contents of modules, and *parametric modules* are
used to abstract over modules (essentially module-level functions).
In Standard ML, modules, module types and parametric modules are
called structs, signatures, and functors, respectively.

Named modules are declared as::

  module name = module expression

A named module type is defined as::

  module type name = module type expression

Where a module expression can be the name of another module, an
application of a parametric module, or a sequence of declarations
enclosed in curly braces::

  module Vec3 = {
    type t = ( f32 , f32 , f32 )
    let add(a: t) (b: t): t =
      let (a1, a2, a3) = a in
      let (b1, b2, b3) = b in
      (a1 + b1, a2 + b2 , a3 + b3)
  }

  module AlsoVec3 = Vec3

Functions and types within modules can be accessed using dot
notation::

    type vector = Vec3.t
    let double(v: vector): vector = Vec3.add v v

We can also use ``open Vec3`` to bring the names defined by ``Vec3``
into the current scope.  Multiple modules can be opened simultaneously
by separating their names with spaces.  In case several modules define
the same names, the ones mentioned last take precedence.  The first
argument to ``open`` may be a full module expression.

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

Parametric modules allow us to write definitions that abstract over
modules.  For example::

  module Times(M: Addable) = {
    let times (x: M.t) (k: int): M.t =
      loop (x' = x) for i < k do
        T.add x' x
  }

We can instantiate ``Times`` with any module that fulfills the module
type ``Addable`` and get back a module that defines a function
``times``::

  module Vec3Times = Times(Vec3)

Now ``Vec3Times.times`` is a function of type ``Vec3.t -> int ->
Vec3.t``.

Module Expressions
~~~~~~~~~~~~~~~~~~

.. productionlist::
   mod_exp:   `qualid`
          : | `mod_exp` ":" `mod_type_exp`
          : | "\" "(" `id` ":" `mod_type_exp` ")" [":" `mod_type_exp`] "=" `mod_exp`
          : | `mod_exp` `mod_exp`
          : | "(" `mod_exp` ")"
          : | "{" `dec`* "}"
          : | "import" `stringlit`

Module Type Expressions
~~~~~~~~~~~~~~~~~~~~~~~

.. productionlist::
   mod_type_exp:   `qualid`
             : | "{" `spec`* "}"
             : | `mod_type_exp` "with" `qualid` "=" `type`
             : | "(" `mod_type_exp` ")"
             : | "(" `id` ":" `mod_type_exp` ")" "->" `mod_type_exp`
             : | `mod_type_exp` "->" `mod_type_exp`


.. productionlist::
   spec:   "val" `id` `type_param`* ":" `spec_type`
       : | "val" `binop` ":" `spec_type`
       : | "type" `id` `type_param`* "=" `type`
       : | "type `id` `type_param`*
       : | "module" `id` ":" `mod_type_exp`
       : | "include" `mod_type_exp`
   spec_type: `type` | `type` "->" `spec_type`

Referring to Other Files
------------------------

You can refer to external files in a Futhark file like this::

  import "module"

The above will include all top-level definitions from ``module.fut``
is and make them available in the current Futhark program.  The
``.fut`` extension is implied.

You can also include files from subdirectories::

  include "path/to/a/file"

The above will include the file ``path/to/a/file.fut``.  When
importing a nonlocal file (such as the standard library or the
compiler search path), the path must begin with a forward slash.

Qualified imports are also possible, where a module is created for the
file::

  module M = import "module"

Literal Defaults
----------------

.. productionlist::
   default_dec:   "default" (`int_type`)
              : | "default" (`float_type`)
              : | "default" (`int_type`, `float_type`)

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
