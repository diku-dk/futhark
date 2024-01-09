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
practice (for example by applying rules of operator precedence).

This reference describes only the language itself.  Documentation for
the built-in prelude is `available elsewhere
<https://futhark-lang.org/docs/prelude>`_.

Comments
--------

Line comments are indicated with ``--`` and continue until end of
line.  A contiguous block of line comments beginning with ``-- |`` is
a *documentation comment* and has special meaning to documentation
tools.  Documentation comments are only allowed immediately before
declarations.

Trailing commas
---------------

All syntactical elements that involve comma-separated sequencing
permit an optional trailing comma.

Identifiers and Keywords
------------------------

.. productionlist::
   name: `letter` `constituent`* | "_" `constituent`*
   constituent: `letter` | `digit` | "_" | "'"
   quals: (`name` ".")+
   qualname: `name` | `quals` `name`
   symbol: `symstartchar` `symchar`*
   qualsymbol: `symbol` | `quals` `symbol` | "`" `qualname` "`"
   fieldid: `decimal` | `name`
   symstartchar: "+" | "-" | "*" | "/" | "%" | "=" | "!" | ">" | "<" | "|" | "&" | "^"
   symchar: `symstartchar` | "."
   constructor: "#" `name`

Many things in Futhark are named. When we are defining something, we
give it an unqualified name (`name`).  When referencing something
inside a module, we use a qualified name (`qualname`).  We can also
use symbols (`symbol`, `qualsymbol`), which are treated as infix by
the grammar.

The constructor names of a sum type are identifiers prefixed with
``#``, with no space afterwards.  The fields of a record are named
with `fieldid`.  Note that a `fieldid` can be a decimal number.
Futhark has three distinct name spaces: terms, module types, and
types.  Modules (including parametric modules) and values both share
the term namespace.

.. _reserved:

Reserved names and symbols
~~~~~~~~~~~~~~~~~~~~~~~~~~

A reserved name or symbol may be used only when explicitly present in
the grammar.  In particular, they cannot be bound in definitions.

The following identifier are reserved: ``true``, ``false``, ``if``,
``then``, ``else``, ``def``, ``let``, ``loop``, ``in``, ``val``,
``for``, ``do``, ``with``, ``local``, ``open``, ``include``,
``import``, ``type``, ``entry``, ``module``, ``while``, ``assert``,
``match``, ``case``.

The following symbols are reserved: ``=``.

.. _primitives:

Primitive Types and Values
--------------------------

.. productionlist::
   literal: `intnumber` | `floatnumber` | "true" | "false"

Boolean literals are written ``true`` and ``false``.  The primitive
types in Futhark are the signed integer types ``i8``, ``i16``,
``i32``, ``i64``, the unsigned integer types ``u8``, ``u16``, ``u32``,
``u64``, the floating-point types ``f16``, ``f32``, ``f64``, as well
as ``bool``.

.. productionlist::
   int_type: "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64"
   float_type: "f16" | "f32" | "f64"

Numeric literals can be suffixed with their intended type.  For
example ``42i8`` is of type ``i8``, and ``1337e2f64`` is of type
``f64``.  If no suffix is given, the type of the literal will be
inferred based on its use.  If the use is not constrained, integral
literals will be assigned type ``i32``, and decimal literals type
``f64``.  Hexadecimal literals are supported by prefixing with ``0x``,
and binary literals by prefixing with ``0b``.

Floats can also be written in hexadecimal format such as ``0x1.fp3``,
instead of the usual decimal notation. Here, ``0x1.f`` evaluates to
``1 15/16`` and the ``p3`` multiplies it by ``2^3 = 8``.

.. productionlist::
   intnumber: (`decimal` | `hexadecimal` | `binary`) [`int_type`]
   decimal: `decdigit` (`decdigit` |"_")*
   hexadecimal: 0 ("x" | "X") `hexdigit` (`hexdigit` |"_")*
   binary: 0 ("b" | "B") `bindigit` (`bindigit` | "_")*

.. productionlist::
   floatnumber: (`pointfloat` | `exponentfloat` | `hexadecimalfloat`) [`float_type`]
   pointfloat: [`intpart`] `fraction`
   exponentfloat: (`intpart` | `pointfloat`) `exponent`
   hexadecimalfloat: 0 ("x" | "X") `hexintpart` `hexfraction` ("p"|"P") ["+" | "-"] `decdigit`+
   intpart: `decdigit` (`decdigit` |"_")*
   fraction: "." `decdigit` (`decdigit` |"_")*
   hexintpart: `hexdigit` (`hexdigit` | "_")*
   hexfraction: "." `hexdigit` (`hexdigit` |"_")*
   exponent: ("e" | "E") ["+" | "-"] `decdigit`+

.. productionlist::
   decdigit: "0"..."9"
   hexdigit: `decdigit` | "a"..."f" | "A"..."F"
   bindigit: "0" | "1"

Compound Types and Values
~~~~~~~~~~~~~~~~~~~~~~~~~

.. productionlist::
   type:   `qualname`
       : | `array_type`
       : | `tuple_type`
       : | `record_type`
       : | `sum_type`
       : | `function_type`
       : | `type_application`
       : | `existential_size`

Compound types can be constructed based on the primitive types.  The
Futhark type system is entirely structural, and type abbreviations are
merely shorthands.  The only exception is abstract types whose
definition has been hidden via the module system (see
:ref:`module-system`).

.. productionlist::
   tuple_type: "(" ")" | "(" `type` ("," `type`)+ [","] ")"

A tuple value or type is written as a sequence of comma-separated
values or types enclosed in parentheses. For example, ``(0, 1)`` is a
tuple value of type ``(i32,i32)``. The elements of a tuple need not
have the same type -- the value ``(false, 1, 2.0)`` is of type
``(bool, i32, f64)``. A tuple element can also be another tuple, as in
``((1,2),(3,4))``, which is of type ``((i32,i32),(i32,i32))``. A tuple
cannot have just one element, but empty tuples are permitted, although
they are not very useful. Empty tuples are written ``()`` and are of
type ``()``.

.. productionlist::
   array_type: "[" [`exp`] "]" `type`

An array value is written as a sequence of zero or more
comma-separated values enclosed in square brackets: ``[1,2,3]``.  An
array type is written as ``[d]t``, where ``t`` is the element type of
the array, and ``d`` is an expression of type ``i64`` indicating the
number of elements in the array.  We can elide ``d`` and write just
``[]`` (an :term:`anonymous size`), in which case the size will be
inferred.  An anonymous size is a syntactic shorthand, and is always
replaced by an actual size by the type checker (either via inference
or by inventing a new name, depending on context).

As an example, an array of three integers could be written as
``[1,2,3]``, and has type ``[3]i32``.  An empty array is written as
``[]``, and its type is inferred from its use.  When writing Futhark
values for such uses as ``futhark test`` (but not when writing
programs), empty arrays are written ``empty([0]t)`` for an empty array
of type ``[0]t``.  When using ``empty``, all dimensions must be given
a size, and at least one must be zero, e.g. ``empty([2][0]i32)``.

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

.. productionlist::
   sum_type: `constructor` `type`* ("|" `constructor` `type`*)*

Sum types are anonymous in Futhark, and are written as the
constructors separated by vertical bars.  Each constructor consists of
a ``#``-prefixed *name*, followed by zero or more types, called its
*payload*.  **Note:** The current implementation of sum types is
fairly inefficient, in that all possible constructors of a sum-typed
value will be resident in memory.  Avoid using sum types where
multiple constructors have large payloads.

.. productionlist::
   record_type: "{" "}" | "{" `fieldid` ":" `type` ("," `fieldid` ":" `type`)* [","] "}"

Records are mappings from field names to values, with the field names
known statically. A tuple behaves in all respects like a record with
numeric field names starting from zero, and vice versa. It is an error
for a record type to name the same field twice. A trailing comma is
permitted.

.. productionlist::
   type_application: `type` `type_arg` | "*" `type`
   type_arg: "[" [`dim`] "]" | `type`

A parametric type abbreviation can be applied by juxtaposing its name
and its arguments.  The application must provide as many arguments as
the type abbreviation has parameters - partial application is
presently not allowed.  See `Type Abbreviations`_ for further details.

.. productionlist::
   function_type: `param_type` "->" `type`
   param_type: `type` | "(" `name` ":" `type` ")"

Functions are classified via function types, but they are not fully
first class.  See :ref:`hofs` for the details.

.. productionlist::
   stringlit: '"' `stringchar`* '"'
   stringchar: <any source character except "\" or newline or double quotes>
   charlit: "'" `char` "'"
   char: <any source character except "\" or newline or single quotes>

String literals are supported, but only as syntactic sugar for UTF-8
encoded arrays of ``u8`` values.  There is no character type in
Futhark, but character literals are interpreted as integers of the
corresponding Unicode code point.

.. productionlist::
   existential_size: "?" ("[" `name` "]")+ "." `type`

An existential size quantifier brings an unknown size into scope
within a type.  This can be used to encode constraints for statically
unknown array sizes.

Declarations
------------

A Futhark module consists of a sequence of declarations.  Files are
also modules.  Each declaration is processed in order, and a
declaration can only refer to names bound by preceding declarations.

.. productionlist::
   dec:   `val_bind` | `type_bind` | `mod_bind` | `mod_type_bind`
      : | "open" `mod_exp`
      : | "import" `stringlit`
      : | "local" `dec`
      : | "#[" `attr` "]" `dec`

Any names defined by a declaration inside a module are by default
visible to users of that module (see :ref:`module-system`).

* ``open mod_exp`` brings names bound in ``mod_exp`` into the current scope.
  These names will also be visible to users of the module.

* ``local dec`` has the meaning of ``dec``, but any names bound by
  ``dec`` will not be visible outside the module.

* ``import "foo"`` is a shorthand for ``local open import "foo"``,
  where the ``import`` is interpreted as a module expression (see
  :ref:`module-system`).

* ``#[attr] dec`` adds an attribute to a declaration (see :ref:`attributes`).

Declaring Functions and Values
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. productionlist::
   val_bind:   ("def" | "entry" | "let") (`name` | "(" `symbol` ")") `type_param`* `pat`* [":" `type`] "=" `exp`
           : | ("def" | "entry" | "let") `pat` `symbol` `pat` [":" `type`] "=" `exp`

**Note:** using ``let`` to define top-level bindings is deprecated.

Functions and constants must be defined before they are used.  A function
declaration must specify the name, parameters, and body
of the function::

  def name params...: rettype = body

Hindley-Milner-style type inference is supported.  A parameter may be
given a type with the notation ``(name: type)``.  Functions may not be
recursive.  The sizes of the arguments can be constrained - see `Size
Types`_.  A function can be *polymorphic* by using type parameters, in
the same way as for `Type Abbreviations`_::

  def reverse [n] 't (xs: [n]t): [n]t = xs[::-1]

Type parameters for a function do not need to cover the types of all
parameters.  The type checker will add more if necessary.  For
example, the following is well typed::

  def pair 'a (x: a) y = (x, y)

A new type variable will be invented for the parameter ``y``.

Shape and type parameters are not passed explicitly when calling
function, but are automatically derived.  If an array value *v* is
passed for a type parameter *t*, all other arguments passed of type
*t* must have the same shape as *v*.  For example, consider the following
definition::

  def pair 't (x: t) (y: t) = (x, y)

The application ``pair [1] [2,3]`` is ill-typed.

To simplify the handling of in-place updates (see
:ref:`in-place-updates`), the value returned by a function may not
alias any global variables.

User-Defined Operators
~~~~~~~~~~~~~~~~~~~~~~

Infix operators are defined much like functions::

  def (p1: t1) op (p2: t2): rt = ...

For example::

  def (a:i32,b:i32) +^ (c:i32,d:i32) = (a+c, b+d)

We can also define operators by enclosing the operator name in
parentheses and suffixing the parameters, as an ordinary function::

  def (+^) (a:i32,b:i32) (c:i32,d:i32) = (a+c, b+d)

This is necessary when defining a polymorphic operator.

A valid operator name is a non-empty sequence of characters chosen
from the string ``"+-*/%=!><&^"``.  The fixity of an operator is
determined by its first characters, which must correspond to a
built-in operator.  Thus, ``+^`` binds like ``+``, whilst ``*^`` binds
like ``*``.  The longest such prefix is used to determine fixity, so
``>>=`` binds like ``>>``, not like ``>``.

It is not permitted to define operators with the names ``&&`` or
``||`` (although these as prefixes are accepted).  This is because a
user-defined version of these operators would not be short-circuiting.
User-defined operators behave exactly like ordinary functions, except
for being infix.

A built-in operator can be shadowed (i.e. a new ``+`` can be defined).
This will result in the built-in polymorphic operator becoming
inaccessible, except through the ``intrinsics`` module.

An infix operator can also be defined with prefix notation, like an
ordinary function, by enclosing it in parentheses::

  def (+) (x: i32) (y: i32) = x - y

This is necessary when defining operators that take type or shape
parameters.

.. _entry-points:

Entry Points
~~~~~~~~~~~~

Apart from declaring a function with the keyword ``def``, it can also
be declared with ``entry``.  When the Futhark program is compiled any
top-level function declared with ``entry`` will be exposed as an entry
point.  If the Futhark program has been compiled as a library, these
are the functions that will be exposed.  If compiled as an executable,
you can use the ``--entry-point`` command line option of the generated
executable to select the entry point you wish to run.

Any top-level function named ``main`` will always be considered an
entry point, whether it is declared with ``entry`` or not.

The name of an entry point must not contain an apostrophe (``'``),
even though that is normally permitted in Futhark identifiers.

Value Declarations
~~~~~~~~~~~~~~~~~~

A named value/constant can be declared as follows::

  def name: type = definition

The definition can be an arbitrary expression, including function
calls and other values, although they must be in scope before the
value is defined.  If the return type contains any anonymous sizes
(see `Size types`_), new existential sizes will be constructed for
them.

.. _typeabbrevs:

Type Abbreviations
~~~~~~~~~~~~~~~~~~

.. productionlist::
   type_bind: ("type" | "type^" | "type~") `name` `type_param`* "=" `type`
   type_param: "[" `name` "]" | "'" `name` | "'~" `name` | "'^" `name`

Type abbreviations function as shorthands for the purpose of
documentation or brevity.  After a type binding ``type t1 = t2``, the
name ``t1`` can be used as a shorthand for the type ``t2``.  Type
abbreviations do not create distinct types: the types ``t1`` and
``t2`` are entirely interchangeable.

If the right-hand side of a type contains existential sizes, it must
be declared "size-lifted" with ``type~``.  If it (potentially)
contains a function, it must be declared "fully lifted" with
``type^``.  A lifted type can also contain existential sizes.  Lifted
types cannot be put in arrays.  Fully lifted types cannot be returned
from conditional or loop expressions.

A type abbreviation can have zero or more parameters.  A type
parameter enclosed with square brackets is a *size parameter*, and
can be used in the definition as an array size, or as a
size argument to other type abbreviations.  When passing an
argument for a shape parameter, it must be enclosed in square
brackets.  Example::

  type two_intvecs [n] = ([n]i32, [n]i32)

  def x: two_intvecs [2] = (iota 2, replicate 2 0)

When referencing a type abbreviation, size parameters work much like
array sizes.  Like sizes, they can be passed an anonymous size
(``[]``).  All size parameters must be used in the definition of the
type abbreviation.

A type parameter prefixed with a single quote is a *type parameter*.
It is in scope as a type in the definition of the type abbreviation.
Whenever the type abbreviation is used in a type expression, a type
argument must be passed for the parameter.  Type arguments need not be
prefixed with single quotes::

  type two_vecs [n] 't = ([n]t, [n]t)
  type two_intvecs [n] = two_vecs [n] i32
  def x: two_vecs [2] i32 = (iota 2, replicate 2 0)

A *size-lifted type parameter* is prefixed with ``'~``, and a *fully
lifted type parameter* with ``'^``.  These have the same rules and
restrictions as lifted type abbreviations.

Expressions
-----------

Expressions are the basic construct of any Futhark program.  An
expression has a statically determined *type*, and produces a *value*
at runtime.  Futhark is an eager/strict language ("call by value").

The basic elements of expressions are called *atoms*, for example
literals and variables, but also more complicated forms.

.. productionlist::
   atom:   `literal`
       : | `qualname` ("." `fieldid`)*
       : | `stringlit`
       : | `charlit`
       : | "(" ")"
       : | "(" `exp` ")" ("." `fieldid`)*
       : | "(" `exp` ("," `exp`)+ [","] ")"
       : | "{" "}"
       : | "{" `field` ("," `field`)* [","] "}"
       : | `qualname` `slice`
       : | "(" `exp` ")" `slice`
       : | `quals` "." "(" `exp` ")"
       : | "[" `exp` ("," `exp`)* [","] "]"
       : | "(" `qualsymbol` ")"
       : | "(" `exp` `qualsymbol` ")"
       : | "(" `qualsymbol` `exp` ")"
       : | "(" ( "." `field` )+ ")"
       : | "(" "." `slice` ")"
       : | "???"
   exp:   `atom`
      : | `exp` `qualsymbol` `exp`
      : | `exp` `exp`
      : | "!" `exp`
      : | "-" `exp`
      : | `constructor` `exp`*
      : | `exp` ":" `type`
      : | `exp` ":>" `type`
      : | `exp` [ ".." `exp` ] "..." `exp`
      : | `exp` [ ".." `exp` ] "..<" `exp`
      : | `exp` [ ".." `exp` ] "..>" `exp`
      : | "if" `exp` "then" `exp` "else" `exp`
      : | "let" `size`* `pat` "=" `exp` "in" `exp`
      : | "let" `name` `slice` "=" `exp` "in" `exp`
      : | "let" `name` `type_param`* `pat`+ [":" `type`] "=" `exp` "in" `exp`
      : | "(" "\" `pat`+ [":" `type`] "->" `exp` ")"
      : | "loop" `pat` ["=" `exp`] `loopform` "do" `exp`
      : | "#[" `attr` "]" `exp`
      : | "unsafe" `exp`
      : | "assert" `atom` `atom`
      : | `exp` "with" `slice` "=" `exp`
      : | `exp` "with" `fieldid` ("." `fieldid`)* "=" `exp`
      : | "match" `exp` ("case" `pat` "->" `exp`)+
   slice: "[" `index` ("," `index`)* [","] "]"
   field:   `fieldid` "=" `exp`
        : | `name`
   size : "[" `name` "]"
   pat:   `name`
      : | `pat_literal`
      : | "_"
      : | "(" ")"
      : | "(" `pat` ")"
      : | "(" `pat` ("," `pat`)+ [","] ")"
      : | "{" "}"
      : | "{" `fieldid` ["=" `pat`] ("," `fieldid` ["=" `pat`])* [","] "}"
      : | `constructor` `pat`*
      : | `pat` ":" `type`
      : | "#[" `attr` "]" `pat`
   pat_literal:   [ "-" ] `intnumber`
              : | [ "-" ] `floatnumber`
              : | `charlit`
              : | "true"
              : | "false"
   loopform :   "for" `name` "<" `exp`
            : | "for" `pat` "in" `exp`
            : | "while" `exp`
   index:   `exp` [":" [`exp`]] [":" [`exp`]]
        : | [`exp`] ":" `exp` [":" [`exp`]]
        : | [`exp`] [":" `exp`] ":" [`exp`]

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

* An expression ``x.y`` may either be a reference to the name ``y`` in
  the module ``x``, or the field ``y`` in the record ``x``.  Modules
  and values occupy the same name space, so this is disambiguated by
  whether ``x`` is a value or module.

* A type ascription (``exp : type``) cannot appear as an array
  index, as it conflicts with the syntax for slicing.

* In ``f [x]``, there is an ambiguity between indexing the array ``f``
  at position ``x``, or calling the function ``f`` with the singleton
  array ``x``.  We resolve this the following way:

    * If there is a space between ``f`` and the opening bracket, it is
      treated as a function application.

    * Otherwise, it is an array index operation.

* An expression ``(-x)`` is parsed as the variable ``x`` negated and
  enclosed in parentheses, rather than an operator section partially
  applying the infix operator ``-``.

* Prefix operators bind more tighly than infix operators.  Note that
  the only prefix operators are the builtin ``!`` and ``-``, and more
  cannot be defined.  In particular, a user-defined operator beginning
  with ``!`` binds as ``!=``, as on the table below, not as the prefix
  operator ``!``

* Function and type application binds more tightly than infix
  operators.

* ``#foo #bar`` is interpreted as a constructor with a ``#bar``
  payload, not as applying ``#foo`` to ``#bar`` (the latter would be
  semantically invalid anyway).

* `Attributes`_ bind less tightly than any other syntactic construct.

* A type application ``pt [n]t`` is parsed as an application of the
  type constructor ``pt`` to the size argument ``[n]`` and the type
  ``t``.  To pass a single array-typed parameter, enclose it in
  parens.

* The bodies of ``let``, ``if``, and ``loop`` extend as far to the
  right as possible.

* The following table describes the precedence and associativity of
  infix operators in both expressions and type expressions.  All
  operators in the same row have the same precedence.  The rows are
  listed in increasing order of precedence.  Note that not all
  operators listed here are used in expressions; nevertheless, they
  are still used for resolving ambiguities.

  =================  =============
  **Associativity**  **Operators**
  =================  =============
  left               ``,``
  left               ``:``, ``:>``
  left               ```symbol```
  left               ``||``
  left               ``&&``
  left               ``<=`` ``>=`` ``>`` ``<`` ``==`` ``!=`` ``!`` ``=``
  left               ``&`` ``^`` ``|``
  left               ``<<`` ``>>``
  left               ``+`` ``-``
  left               ``*`` ``/`` ``%`` ``//`` ``%%``
  left               ``|>``
  right              ``<|``
  right              ``->``
  left               ``**``
  left               juxtaposition
  =================  =============

.. _patterns:

Patterns
~~~~~~~~

We say that a pattern is *irrefutable* if it can never fail to match a
value of the appropriate type.  Concretely, this means that it does
not require any specific sum type constructor (unless the type in
question has only a single constructor), or any specific numeric or
boolean literal.  Patterns used in function parameters and ``let``
bindings must be irrefutable.  Patterns used in ``case`` need not be
irrefutable.

A pattern ``_`` matches any value.  A pattern consisting of a literal
value (e.g. a numeric constant) matches exactly that value.

Semantics of Simple Expressions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`literal`
.........

Evaluates to itself.

`qualname`
..........

A variable name; evaluates to its value in the current environment.

`stringlit`
...........

Evaluates to an array of type ``[]u8`` that contains the characters
encoded as UTF-8.

``()``
......

Evaluates to an empty tuple.

``( e )``
.........

Evaluates to the result of ``e``.

``???``
.......

A *typed hole*, usable as a placeholder expression.  The type checker
will infer any necessary type for this expression.  This can sometimes
result in an ambiguous type, which can be resolved using a type
ascription.  Evaluating a typed hole results in a run-time error.

``(e1, e2, ..., eN)``
.....................

Evaluates to a tuple containing ``N`` values.  Equivalent to the
record literal ``{0=e1, 1=e2, ..., N-1=eN}``.

``{f1, f2, ..., fN}``
.....................

A record expression consists of a comma-separated sequence of *field
expressions*.  Each field expression defines the value of a field in
the record.  A field expression can take one of two forms:

  ``f = e``: defines a field with the name ``f`` and the value
  resulting from evaluating ``e``.

  ``f``: defines a field with the name ``f`` and the value of the
  variable ``f`` in scope.

Each field may only be defined once.

``a[i]``
........

Return the element at the given position in the array.  The index may
be a comma-separated list of indexes instead of just a single index.
If the number of indices given is less than the rank of the array, an
array is returned.  The index may be of any unsigned integer type.

The array ``a`` must be a variable name or a parenthesised expression.
Furthermore, there *may not* be a space between ``a`` and the opening
bracket.  This disambiguates the array indexing ``a[i]``, from ``a
[i]``, which is a function call with a literal array.

.. _slices:

``a[i:j:s]``
............

Return a slice of the array ``a`` from index ``i`` to ``j``, the
former inclusive and the latter exclusive, taking every ``s``-th
element.  The ``s`` parameter may not be zero.  If ``s`` is negative,
it means to start at ``i`` and descend by steps of size ``s`` to ``j``
(not inclusive).  Slicing can be done only with expressions of type
``i64``.

It is generally a bad idea for ``s`` to be non-constant.
Slicing of multiple dimensions can be done by separating with commas,
and may be intermixed freely with indexing.

If ``s`` is elided it defaults to ``1``.  If ``i`` or ``j`` is elided, their
value depends on the sign of ``s``.  If ``s`` is positive, ``i`` become ``0``
and ``j`` become the length of the array.  If ``s`` is negative, ``i`` becomes
the length of the array minus one, and ``j`` becomes minus one.  This means that
``a[::-1]`` is the reverse of the array ``a``.

In the general case, the size of the array produced by a slice is
unknown (see `Size types`_).  In a few cases, the size is known
statically:

  * ``a[0:n]`` has size ``n``

  * ``a[:n]`` has size ``n``

  * ``a[0:n:1]`` has size ``n``

  * ``a[:n:1]`` has size ``n``

This holds only if ``n`` is a variable or constant.

``[x, y, z]``
.............

Create an array containing the indicated elements.  Each element must
have the same type and shape.

.. _range:

``x..y...z``
............

Construct a signed integer array whose first element is ``x`` and
which proceeds with a stride of ``y-x`` until reaching ``z``
(inclusive).  The ``..y`` part can be elided in which case a stride of
1 is used.  A run-time error occurs if ``z`` is less than ``x`` or
``y``, or if ``x`` and ``y`` are the same value.

In the general case, the size of the array produced by a range is
unknown (see `Size types`_).  In a few cases, the size is known
statically:

  * ``1..2...n`` has size ``n``

This holds only if ``n`` is a variable or constant.

.. _range_upto:

``x..y..<z``
............

Construct a signed integer array whose first elements is ``x``, and
which proceeds upwards with a stride of ``y-x`` until reaching ``z``
(exclusive).  The ``..y`` part can be elided in which case a stride of
1 is used.  A run-time error occurs if ``z`` is less than ``x`` or
``y``, or if ``x`` and ``y`` are the same value.

  * ``0..1..<n`` has size ``n``

  * ``0..<n`` has size ``n``

This holds only if ``n`` is a variable or constant.

``x..y..>z``
...............

Construct a signed integer array whose first elements is ``x``, and
which proceeds downwards with a stride of ``y-x`` until reaching ``z``
(exclusive).  The ``..y`` part can be elided in which case a stride of
-1 is used.  A run-time error occurs if ``z`` is greater than ``x`` or
``y``, or if ``x`` and ``y`` are the same value.

``e.f``
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
"magical" *overloaded* operators that work on several types.
Overloaded operators cannot be defined by the user.  Both operands
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

  ``^``, ``&``, ``|``, ``>>``, ``<<``

    Bitwise operators, respectively bitwise xor, and, or, arithmetic
    shift right and left, and logical shift right.  **Shifting is
    undefined if the right operand is negative, or greater than or
    equal to the length in bits of the left operand.**

    Note that, unlike in C, bitwise operators have *higher* priority
    than arithmetic operators.  This means that ``x & y == z`` is
    understood as ``(x & y) == z``, rather than ``x & (y == z)`` as it
    would in C.  Note that the latter is a type error in Futhark
    anyhow.

  ``==``, ``!=``

    Compare any two values of builtin or compound type for equality.

  ``<``, ``<=``.  ``>``, ``>=``

    Company any two values of numeric type for equality.

  ```qualname```

    Use ``qualname``, which may be any non-operator function name, as
    an infix operator.

``x && y``
..........

Short-circuiting logical conjunction; both operands must be of type
``bool``.

``x || y``
..........

Short-circuiting logical disjunction; both operands must be of type
``bool``.

``f x``
.......

Apply the function ``f`` to the argument ``x``.

``#c x y z``
............

Apply the sum type constructor ``#x`` to the payload ``x``, ``y``, and
``z``.  A constructor application is always assumed to be saturated,
i.e. its entire payload provided.  This means that constructors may
not be partially applied.

``e : t``
.........

Annotate that ``e`` is expected to be of type ``t``, failing with a
type error if it is not.  If ``t`` is an array with shape
declarations, the correctness of the shape declarations is checked at
run-time.

Due to ambiguities, this syntactic form cannot appear as an array
index expression unless it is first enclosed in parentheses.  However,
as an array index must always be of type ``i64``, there is never a
reason to put an explicit type ascription there.

``e :> t``
..........

Coerce the size of ``e`` to ``t``.  The type of ``t`` must match the
type of ``e``, except that the sizes may be statically different.  At
run-time, it will be verified that the sizes are the same.

``! x``
.......

Logical negation if ``x`` is of type ``bool``.  Bitwise negation if
``x`` is of integral type.

``- x``
.......

Numerical negation of ``x``, which must be of numeric type.

``#[attr] e``
.............

Apply the given attribute to the expression.  Attributes are an ad-hoc
and optional mechanism for providing extra information, directives, or
hints to the compiler.  See :ref:`attributes` for more information.

``unsafe e``
............

Elide safety checks and assertions (such as bounds checking) that
occur during execution of ``e``.  This is useful if the compiler is
otherwise unable to avoid bounds checks (e.g. when using indirect
indexes), but you really do not want them there.  Make very sure that
the code is correct; eliding such checks can lead to memory
corruption.

This construct is deprecated.  Use the ``#[unsafe]`` attribute instead.

.. _assert:

``assert cond e``
.................

Terminate execution with an error if ``cond`` evaluates to false,
otherwise produce the result of evaluating ``e``.  Unless ``e``
produces a value that is used subsequently (it can just be a
variable), dead code elimination may remove the assertion.

``a with [i] = e``
...................

Return ``a``, but with the element at position ``i`` changed to
contain the result of evaluating ``e``.  Consumes ``a``.

.. _record_update:

``r with f = e``
.................

Return the record ``r``, but with field ``f`` changed to have value
``e``.  The type of the field must remain unchanged.  Type inference
is limited: ``r`` must have a *completely known type* up to ``f``.
This sometimes requires extra type annotations to make the type of
``r`` known.

``if c then a else b``
......................

If ``c`` evaluates to ``true``, evaluate ``a``, else evaluate ``b``.

Binding Expressions
~~~~~~~~~~~~~~~~~~~

``let pat = e in body``
.......................

Evaluate ``e`` and bind the result to the irrefutable pattern ``pat``
(see :ref:`patterns`) while evaluating ``body``.  The ``in`` keyword
is optional if ``body`` is a ``let`` expression.  The binding is not
let-generalised, meaning it has a monomorphic type.  This can be
significant if ``e`` is of functional type.

If ``e`` is of type ``i64`` and ``pat`` binds only a single name
``v``, then the type of the overall expression is the type of
``body``, but with any occurence of ``v`` replaced by ``e``.

``let [n] pat = e in body``
...........................

As above, but bind sizes (here ``n``) used in the pattern (here to the
size of the array being bound).  All sizes must be used in the
pattern.  Roughly Equivalent to ``let f [n] pat = body in f e``.

``let a[i] = v in body``
........................

Write ``v`` to ``a[i]`` and evaluate ``body``.  The given index need
not be complete and can also be a slice, but in these cases, the value
of ``v`` must be an array of the proper size.  This notation is
Syntactic sugar for ``let a = a with [i] = v in a``.

``let f params... = e in body``
...............................

Bind ``f`` to a function with the given parameters and definition
(``e``) and evaluate ``body``.  The function will be treated as
aliasing any free variables in ``e``.  The function is not in scope of
itself, and hence cannot be recursive.

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

``loop pat = initial for x < n do loopbody``
............................................

Equivalent to ``loop (pat = initial) for x in [0..1..<n] do loopbody``.

``loop pat = initial while cond do loopbody``
...............................................

1. Bind ``pat`` to the initial values given in ``initial``.

2. If ``cond`` evaluates to true, bind ``pat`` to the result of
   evaluating ``loopbody``, and repeat the step.

3. Return the final value of ``pat``.

``match x case p1 -> e1 case p2 -> e2``
.......................................

Match the value produced by ``x`` to each of the patterns in turn,
picking the first one that succeeds.  The result of the corresponding
expression is the value of the entire ``match`` expression.  All the
expressions associated with a ``case`` must have the same type (but
not necessarily match the type of ``x``).  It is a type error if there
is not a ``case`` for every possible value of ``x`` - inexhaustive
pattern matching is not allowed.

Function Expressions
~~~~~~~~~~~~~~~~~~~~

``\x y z: t -> e``
..................

Produces an anonymous function taking parameters ``x``, ``y``, and
``z``, returns type ``t``, and whose body is ``e``.  Lambdas do not
permit type parameters; use a named function if you want a polymorphic
function.

``(binop)``
...........

An *operator section* that is equivalent to ``\x y -> x *binop* y``.

``(x binop)``
.............

An *operator section* that is equivalent to ``\y -> x *binop* y``.

``(binop y)``
.............

An *operator section* that is equivalent to ``\x -> x *binop* y``.

``(.a.b.c)``
............

An *operator section* that is equivalent to ``\x -> x.a.b.c``.

``(.[i,j])``
............

An *operator section* that is equivalent to ``\x -> x[i,j]``.

.. _hofs:

Higher-order functions
----------------------

At a high level, Futhark functions are values, and can be used as any
other value.  However, to ensure that the compiler is able to compile
the higher-order functions efficiently via *defunctionalisation*,
certain type-driven restrictions exist on how functions can be used.
These also apply to any record or tuple containing a function (a
*functional type*):

* Arrays of functions are not permitted.

* A function cannot be returned from an ``if`` expression.

* A ``loop`` parameter cannot be a function.

Further, *type parameters* are divided into *non-lifted* (bound with
an apostrophe, e.g. ``'t``), *size-lifted* (``'~t``), and *fully
lifted* (``'^t``).  Only fully lifted type parameters may be
instantiated with a functional type.  Within a function, a lifted type
parameter is treated as a functional type.

See also `In-place updates`_ for details on how consumption
interacts with higher-order functions.

Type Inference
--------------

Futhark supports Hindley-Milner-style type inference, so in many cases
explicit type annotations can be left off.  Record field projection
cannot in isolation be fully inferred, and may need type annotations
where their inputs are bound.  The same goes when constructing sum
types, as Futhark cannot assume that a given constructor only belongs
to a single type.  Further, consumed parameters (see `In-place updates`_)
must be explicitly annotated.

Type inference processes top-level declared in top-down order, and the
type of a top-level function must be completely inferred at its
definition site.  Specifically, if a top-level function uses
overloaded arithmetic operators, the resolution of those overloads
cannot be influenced by later uses of the function.

Local bindings made with ``let`` are not made polymorphic through
let-generalisation *unless* they are syntactically functions, meaning
they have at least one named parameter.

.. _size-types:

Size Types
----------

Futhark supports a system of size-dependent types that statically
checks that the sizes of arrays passed to a function are compatible.

Whenever a pattern occurs (in ``let``, ``loop``, and function
parameters), as well as in return types, the types of the bindings
express invariants about the shapes of arrays that are accepted or
produced by the function.  For example::

  def f [n] (a: [n]i32) (b: [n]i32): [n]i32 =
    map2 (+) a b

We use a *size parameter*, ``[n]``, to explicitly quantify a size.
The ``[n]`` parameter is not explicitly passed when calling ``f``.
Rather, its value is implicitly deduced from the arguments passed for
the value parameters.  An array type can contain *anonymous sizes*,
e.g. ``[]i32``, for which the type checker will invent fresh size
parameters, which ensures that all arrays have a size.  On the
right-hand side of a function arrow ("return types"), this results in
an *existential size* that is not known until the function is fully
applied, e.g::

  val filter [n] 'a : (p: a -> bool) -> (as: [n]a) -> ?[k].[k]a

Sizes can be any expression of type ``i64`` that does not consume any
free variables.  Size parameters can be used as ordinary variables of
type ``i64`` within the scope of the parameters.  The type checker
verifies that the program obeys any constraints imposed by size
annotations.

*Size-dependent types* are supported, as the names of parameters can
be used in the return type of a function::

  def replicate 't (n: i64) (x: t): [n]t = ...

An application ``replicate 10 0`` will have type ``[10]i32``.

Whenever we write a type ``[e]t``, ``e`` must be a well-typed
expression of type ``i64`` in scope (possibly by referencing names
bound as a size parameter).

.. _unknown-sizes:

Unknown sizes
~~~~~~~~~~~~~

There are cases where the type checker cannot assign a precise size to
the result of some operation.  For example, the type of ``filter``
is::

  val filter [n] 'a : (a -> bool) -> [n]t -> ?[m].[m]t

The function returns of an array of *some existential size* ``m``, but
it cannot be known in advance.

When an application ``filter p xs`` is found, the result will be of
type ``[k]t``, where ``k`` is a fresh *unknown size* that is
considered distinct from every other size in the program.  It is
sometimes necessary to perform a size coercion (see `Size coercion`_)
to convert an unknown size to a known size.

Generally, unknown sizes are constructed whenever the true size cannot
be expressed.  The following lists all possible sources of unknown
sizes.

Size going out of scope
.......................

An unknown size is created in some cases when the a type references a
name that has gone out of scope::

  match ...
  case #some c -> replicate c 0

The type of ``replicate c 0`` is ``[c]i32``, but since ``c`` is
locally bound, the type of the entire expression is ``[k]i32`` for
some fresh ``k``.

Consuming expression passed as function argument
................................................

The type of ``replicate e 0`` should be ``[e]i32``, but if ``e`` is an
expression that is not valid as a size, this is not expressible.
Therefore an unknown size ``k`` is created and the size of the
expression becomes ``[k]i32``.

Compound expression used as range bound
.......................................

While a simple range expression such as ``0..<n`` can be assigned type
``[n]i32``, a range expression ``0..<(n+1)`` will give produce an
unknown size.

Complex slicing
...............

Most complex array slicing, such as ``xs[a:b]``, will have an unknown
size.  Exceptions are listed in the :ref:`reference for slice
expressions <slices>`.

Complex ranges
..............

Most complex ranges, such as ``a..<b``, will have an unknown size.
Exceptions exist for :ref:`general ranges <range>` and :ref:`"upto"
ranges <range_upto>`.

Existential size in function return type
........................................

Whenever the result of a function application has an existential
size, that size is replaced with a fresh unknown size variable.

For example, ``filter`` has the following type::

  val filter [n] 'a : (p: a -> bool) -> (as: [n]a) -> ?[k].[k]a

For an application ``filter f xs``, the type checker invents a fresh
unknown size ``k'``, and the actual type for this specific application
will be ``[k']a``.

Branches of ``if`` return arrays of different sizes
...................................................

When an ``if`` (or ``match``) expression has branches that returns
array of different sizes, the differing sizes will be replaced with
fresh unknown sizes.  For example::

  if b then [[1,2], [3,4]]
       else [[5,6]]

This expression will have type ``[k][2]i32``, for some fresh ``k``.

**Important:** The check whether the sizes differ is done when first
encountering the ``if`` or ``match`` during type checking.  At this
point, the type checker may not realise that the two sizes are
actually equal, even though constraints later in the function force
them to be.  This can always be resolved by adding type annotations.

An array produced by a loop does not have a known size
......................................................

If the size of some loop parameter is not maintained across a loop
iteration, the final result of the loop will contain unknown sizes.
For example::

  loop xs = [1] for i < n do xs ++ xs

Similar to conditionals, the type checker may sometimes be too
cautious in assuming that some size may change during the loop.
Adding type annotations to the loop parameter can be used to resolve
this.

.. _size-coercion:

Size coercion
~~~~~~~~~~~~~

Size coercion, written with ``:>``, can be used to perform a
runtime-checked coercion of one size to another.  This can be useful
as an escape hatch in the size type system::

  def concat_to 'a (m: i32) (a: []a) (b: []a) : [m]a =
    a ++ b :> [m]a

.. _causality:

Causality restriction
~~~~~~~~~~~~~~~~~~~~~

Conceptually, size parameters are assigned their value by reading the
sizes of concrete values passed along as parameters.  This means that
any size parameter must be used as the size of some parameter.  This
is an error::

  def f [n] (x: i32) = n

The following is not an error::

  def f [n] (g: [n]i32 -> [n]i32) = ...

However, using this function comes with a constraint: whenever an
application ``f x`` occurs, the value of the size parameter must be
inferable.  Specifically, this value must have been used as the size
of an array *before* the ``f x`` application is encountered.  The
notion of "before" is subtle, as there is no evaluation ordering of a
Futhark expression, *except* that a ``let``-binding is always
evaluated before its body, the argument to a function is always
evaluated before the function itself, and the left operand to an
operator is evaluated before the right.

The causality restriction only occurs when a function has size
parameters whose first use is *not* as a concrete array size.  For
example, it does not apply to uses of the following function::

  def f [n] (arr: [n]i32) (g: [n]i32 -> [n]i32) = ...

This is because the proper value of ``n`` can be read directly from
the actual size of the array.

Empty array literals
~~~~~~~~~~~~~~~~~~~~

Just as with size-polymorphic functions, when constructing an empty
array, we must know the exact size of the (missing) elements.  For
example, in the following program we are forcing the elements of ``a``
to be the same as the elements of ``b``, but the size of the elements
of ``b`` are not known at the time ``a`` is constructed::

  def main (b: bool) (xs: []i32) =
    let a = [] : [][]i32
    let b = [filter (>0) xs]
    in a[0] == b[0]

The result is a type error.

Sum types
~~~~~~~~~

When constructing a value of a sum type, the compiler must still be
able to determine the size of the constructors that are *not* used.
This is illegal::

  type sum = #foo ([]i32) | #bar ([]i32)

  def main (xs: *[]i32) =
    let v : sum = #foo xs
    in xs

Modules
~~~~~~~

When matching a module with a module type (see :ref:`module-system`),
a non-lifted abstract type (i.e. one that is declared with ``type``
rather than ``type^``) may not be implemented by a type abbreviation
that contains any existential sizes.  This is to ensure that if we
have the following::

  module m : { type t } = ...

Then we can construct an array of values of type ``m.t`` without
worrying about constructing an irregular array.

Higher-order functions
~~~~~~~~~~~~~~~~~~~~~~

When a higher-order function takes a functional argument whose return
type is a non-lifted type parameter, any instantiation of that type
parameter must have a non-existential size.  If the return type is a
lifted type parameter, then the instantiation may contain existential
sizes.  This is why the type of ``map`` guarantees regular arrays::

  val map [n] 'a 'b : (a -> b) -> [n]a -> [n]b

The type parameter ``b`` can only be replaced with a type that has
non-existential sizes, which means they must be the same for every
application of the function.  In contrast, this is the type of the
pipeline operator::

  val (|>) '^a -> '^b : a -> (a -> b) -> b

The provided function can return something with an existential size
(such as ``filter``).

A function whose return type has an unknown size
................................................

If a function (named or anonymous) is inferred to have a return type
that contains an unknown size variable created *within* the function
body, that size variable will be replaced with an existential size.  In
most cases this is not important, but it means that an expression like
the following is ill-typed::

  map (\xs -> iota (length xs)) (xss : [n][m]i32)

This is because the ``(length xs)`` expression gives rise to some
fresh size ``k``.  The lambda is then assigned the type ``[n]t ->
[k]i32``, which is immediately turned into ``[n]t -> ?[k].[k]i32`` because
``k`` was generated inside its body.  A function of this type cannot
be passed to ``map``, as explained before.  The solution is to bind
``length`` to a name *before* the lambda.


.. _in-place-updates:

In-place Updates
----------------

In-place updates do not provide observable side effects, but they do
provide a way to efficiently update an array in-place, with the
guarantee that the cost is proportional to the size of the value(s)
being written, not the size of the full array.

The ``a with [i] = v`` language construct, and derived forms,
performs an in-place update.  The compiler verifies that the original
array (``a``) is not used on any execution path following the in-place
update.  This involves also checking that no *alias* of ``a`` is used.
Generally, most language constructs produce new arrays, but some
(slicing) create arrays that alias their input arrays.

When defining a function parameter we can mark it as *consuming* by
prefixing it with an asterisk.  For a return type, we can mark it as
*alias-free* by prefixing it with an asterisk.  For example::

  def modify (a: *[]i32) (i: i32) (x: i32): *[]i32 =
    a with [i] = a[i] + x

A parameter that is not consuming is called *observing*.  In the
parameter declaration ``a: *[i32]``, the asterisk means that the
function ``modify`` has been given "ownership" of the array ``a``,
meaning that any caller of ``modify`` will never reference array ``a``
after the call again.  This allows the ``with`` expression to perform
an in-place update.  After a call ``modify a i x``, neither ``a`` or
any variable that *aliases* ``a`` may be used on any following
execution path.

If an asterisk is present at *any point* inside a tuple parameter
type, the parameter as a whole is considered consuming.  For example::

  def consumes_both ((a,b): (*[]i32,[]i32)) = ...

This is usually not desirable behaviour.  Use multiple parameters
instead::

  def consumes_first_arg (a: *[]i32) (b: []i32) = ...

For bulk in-place updates with multiple values, use the ``scatter``
function in the basis library.

Alias Analysis
~~~~~~~~~~~~~~

The rules used by the Futhark compiler to determine aliasing are
intuitive in the intra-procedural case.  Aliases are associated with
entire arrays.  Aliases of a record are tuple are tracked for each
element, not for the record or tuple itself.  Most constructs produce
fresh arrays, with no aliases.  The main exceptions are ``if``,
``loop``, function calls, and variable literals.

* After a binding ``let a = b``, that simply assigns a new name to an
  existing variable, the variable ``a`` aliases ``b``.  Similarly for
  record projections and patterns.

* The result of an ``if`` aliases the union of the aliases of the
  components.

* The result of a ``loop`` aliases the initial values, as well as any
  aliases that the merge parameters may assume at the end of an
  iteration, computed to a fixed point.

* The aliases of a value returned from a function is the most
  interesting case, and depends on whether the return value is
  declared *alias-free* (with an asterisk ``*``) or not.  If it is
  declared alias-free, then it has no aliases.  Otherwise, it aliases
  all arguments passed for *non-consumed* parameters.

In-place Updates and Higher-Order Functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Consumption generally interacts inflexibly with higher-order
functions.  The issue is that we cannot control how many times a
function argument is applied, or to what, so it is not safe to pass a
function that consumes its argument.  The following two conservative
rules govern the interaction between consumption and higher-order
functions:

1. In the expression ``let p = e1 in ...``, if *any* in-place update
   takes place in the expression ``e1``, the value bound by ``p`` must
   not be or contain a function.

2. A function that consumes one of its arguments may not be passed as
   a higher-order argument to another function.

.. _module-system:

Modules
-------

.. productionlist::
   mod_bind: "module" `name` `mod_param`* "=" [":" `mod_type_exp`] "=" `mod_exp`
   mod_param: "(" `name` ":" `mod_type_exp` ")"
   mod_type_bind: "module" "type" `name` "=" `mod_type_exp`

Futhark supports an ML-style higher-order module system. *Modules* can
contain types, functions, and other modules and module types. *Module
types* are used to classify the contents of modules, and *parametric
modules* are used to abstract over modules (essentially module-level
functions). In Standard ML, modules, module types and parametric
modules are called *structs*, *signatures*, and *functors*,
respectively. Module names exist in the same name space as values, but
module types are their own name space.

Module bindings
~~~~~~~~~~~~~~~

``module m = mod_exp``
......................

Binds *m* to the module produced by the module expression ``mod_exp``.
Any name x in the module produced by ``mod_exp`` can then be accessed
with ``m.x``.

``module m : mod_type_exp = mod_exp``
.....................................

Shorthand for ``module m = mod_exp : mod_type_exp``.

``module m mod_params... = mod_exp``
....................................

Shorthand for ``module m = \mod_params... -> mod_exp``.  This produces
a parametric module.

``module type mt = mod_type_exp``
.................................

Binds *mt* to the module type produced by the module type expression
``mod_type_exp``.

Module Expressions
~~~~~~~~~~~~~~~~~~

.. productionlist::
   mod_exp:   `qualname`
          : | `mod_exp` ":" `mod_type_exp`
          : | "\" "(" `mod_param`* ")" [":" `mod_type_exp`] "->" `mod_exp`
          : | `mod_exp` `mod_exp`
          : | "(" `mod_exp` ")"
          : | "{" `dec`* "}"
          : | "import" `stringlit`

A module expression produces a module.  Modules are collections of
bindings produced by declarations (`dec`).  In particular, a module
may contain other modules or module types.

``qualname``
............

Evaluates to the module of the given name.

``(mod_exp)``
.............

Evaluates to ``mod_exp``.

``mod_exp : mod_type_exp``
..........................

*Module ascription* evaluates the module expression and the module
type expression, verifies that the module implements the module type,
then returns a module that exposes only the functionality described by
the module type.  This is how internal details of a module can be
hidden.

``\(p: mt1): mt2 -> e``
.......................

Constructs a *parametric module* (a function at the module level) that
accepts a parameter of module type ``mt1`` and returns a module of
type ``mt2``.  The latter is optional, but the parameter type is not.

``e1 e2``
.........

Apply the parametric module ``m1`` to the module ``m2``.

``{ decs }``
............

Returns a module that contains the given definitions.  The resulting
module defines any name defined by any declaration that is not
``local``, *in particular* including names made available via
``open``.

``import "foo"``
................

Returns a module that contains the definitions of the file ``"foo"``
relative to the current file.

Module Type Expressions
~~~~~~~~~~~~~~~~~~~~~~~

.. productionlist::
   mod_type_exp:   `qualname`
             : | "{" `spec`* "}"
             : | `mod_type_exp` "with" `qualname` `type_param`* "=" `type`
             : | "(" `mod_type_exp` ")"
             : | "(" `name` ":" `mod_type_exp` ")" "->" `mod_type_exp`
             : | `mod_type_exp` "->" `mod_type_exp`


.. productionlist::
   spec:   "val" `name` `type_param`* ":" `type`
       : | "val" `symbol` `type_param`* ":" `type`
       : | ("type" | "type^" | "type~") `name` `type_param`* "=" `type`
       : | ("type" | "type^" | "type~") `name` `type_param`*
       : | "module" `name` ":" `mod_type_exp`
       : | "include" `mod_type_exp`
       : | "#[" `attr` "]" `spec`

Module types classify modules, with the only (unimportant) difference
in expressivity being that modules can contain module types, but
module types cannot specify that a module must contain a specific
module type. They can specify of course that a module contains a
*submodule* of a specific module type.

A module type expression can be the name of another module type, or a
sequence of *specifications*, or *specs*, enclosed in curly braces.  A
spec can be a *value spec*, indicating the presence of a function or
value, an *abstract type spec*, or a *type abbreviation spec*.

In a value spec, sizes in types on the left-hand side of a function
arrow must not be anonymous.  For example, this is forbidden::

  val sum: []t -> t

Instead write::

  val sum [n]: [n]t -> t

But this is allowed, because the empty size is not to the left of a
function arrow::

  val evens [n]: [n]i32 -> []i32

.. _other-files:

Referencing Other Files
-----------------------

You can refer to external files in a Futhark file like this::

  import "file"

The above will include all non-``local`` top-level definitions from
``file.fut`` is and make them available in the current file (but
will not export them).  The ``.fut`` extension is implied.

You can also include files from subdirectories::

  import "path/to/a/file"

The above will include the file ``path/to/a/file.fut`` relative to the
including file.

Qualified imports are also possible, where a module is created for the
file::

  module M = import "file"

In fact, a plain ``import "file"`` is equivalent to::

  local open import "file"

To re-export names from another file in the current module, use::

  open import "file"

.. _attributes:

Attributes
----------

.. productionlist::
   attr:   `name`
       : | `decimal`
       : | `name` "(" [`attr` ("," `attr`)* [","]] ")"

An expression, declaration, pattern, or module type spec can be
prefixed with an attribute, written as ``#[attr]``.  This may affect
how it is treated by the compiler or other tools.  In no case will
attributes affect or change the *semantics* of a program, but it may
affect how well it compiles and runs (or in some cases, whether it
compiles or runs at all).  Unknown attributes are silently ignored.
Most have no effect in the interpreter.  An attribute can be either an
*atom*, written as an identifier or number, or *compound*, consisting
of an identifier and a comma-separated sequence of attributes.  The
latter is used for grouping and encoding of more complex information.

Expression attributes
~~~~~~~~~~~~~~~~~~~~~

Many expression attributes affect second-order array combinators
(*SOACS*).  These must be applied to a fully saturated function
application or they will have no effect.  If two SOACs with
contradictory attributes are combined through fusion, it is
unspecified which attributes take precedence.

The following expression attributes are supported.

``trace``
.........

Print the value produced by the attributed expression.  Used for
debugging.  Somewhat unreliable outside of the interpreter, and in
particular does not work for GPU device code.

``trace(tag)``
..............

Like ``trace``, but prefix output with *tag*, which must lexically be
an identifier.

``break``
.........

In the interpreter, pause execution *before* evaluating the expression.
No effect for compiled code.

``opaque``
..........

The compiler will treat the attributed expression as a black box.
This is used to work around optimisation deficiencies (or bugs),
although it should hopefully rarely be necessary.

``incremental_flattening(no_outer)``
....................................

When using incremental flattening, do not generate the "only outer
parallelism" version for the attributed SOACs.

``incremental_flattening(no_intra)``
....................................

When using incremental flattening, do not generate the "intra-block
parallelism" version for the attributed SOACs.

``incremental_flattening(only_intra)``
......................................

When using incremental flattening, *only* generate the "intra-block
parallelism" version of the attributed SOACs.  **Beware**: the
resulting program will fail to run if the inner parallelism does not
fit on the device.

``incremental_flattening(only_inner)``
......................................

When using incremental flattening, do not generate multiple versions
for this SOAC, but do exploit inner parallelism (which may give rise
to multiple versions at deeper levels).

``noinline``
............

Do not inline the attributed function application.  If used within a
parallel construct (e.g. ``map``), this will likely prevent the GPU
backends from generating working code.

``sequential``
..............

*Fully* sequentialise the attributed SOAC.

``sequential_outer``
....................

Turn the outer parallelism in the attributed SOAC sequential, but
preserve any inner parallelism.

``sequential_inner``
....................

Exploit only outer parallelism in the attributed SOAC.

``unroll``
..........

Fully unroll the attributed ``loop``.  If the compiler cannot
determine the exact number of iterations (possibly after other
optimisations and simplifications have taken place), then this
attribute has no code generation effect, but instead results in a
warning.  Be very careful with this attribute: it can massively
increase program size (possibly crashing the compiler) if the loop has
a huge number of iterations.

``unsafe``
..........

Do not perform any dynamic safety checks (such as bound checks) during
execution of the attributed expression.

``warn(safety_checks)``
.......................

Make the compiler issue a warning if the attributed expression (or its
subexpressions) requires safety checks (such as bounds checking) at
run-time.  This is used for performance-critical code where you want
to be told when the compiler is unable to statically verify the safety
of all operations.

Declaration attributes
~~~~~~~~~~~~~~~~~~~~~~

The following declaration attributes are supported.

``noinline``
............

Do not inline any calls to this function.  If the function is then
used within a parallel construct (e.g. ``map``), this will likely
prevent the GPU backends from generating working code.

``inline``
..........

Always inline calls to this function.

Pattern attributes
~~~~~~~~~~~~~~~~~~

No pattern attributes are currently supported by the compiler itself,
although they are syntactically permitted and may be used by other
tools.

Spec attributes
~~~~~~~~~~~~~~~

No spec attributes are currently supported by the compiler itself,
although they are syntactically permitted and may be used by other
tools.
