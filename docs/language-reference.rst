.. _language-reference:

Language Reference
==================

The builtin types in Futhark are ``int``, ``real``, ``bool`` and
``char``, as well as their combination in tuples and arrays.  An
``int`` is currently 32 bits and ``real`` is a double-precision double
(64 bits).  This is likely to become configurable in the future.

The following list describes every syntactical language construct in
the language.  For convenience, we will sometimes talk of expressions
"modifying" the input.  As Futhark is a pure language, what happens is
of course that a new array is returned instead of the original being
modified.

Function Declarations
---------------------

A function declaration must specify the return type, name, parameters,
and body of the function::

  fun rettype_type name(params...) = body

Type inference is not supported, and the function is fully
monomorphic.  Optionally, the programmer may put *shape declarations*
in the return type and parameter types.  These can be used to express
invariants about the shapes of arrays that are accepted or produced by
the function, e.g::

  fun [int,!n] f([int,n] a) =
    map(+1, a)

The above declaration specifies a function that takes an array
containing ``n`` elements and returns an array likewise containing
``n`z elements.  The ``!`` is used to indicate that we are referring
to the already bound ``n``, rather than a fresh ``n``.  Forgetting the
``!`` is a very common error, so be careful.

The same name can be used in several dimensions, or even in several
parameters.  This can be used to give a natural type to a function for
computing dot products::

  fun real dotProduct([real,n] a, [real,n] b) =
    reduce(+, 0, zipWith(*, a, b))

Or matrix multiplication::

  fun [[int,!n],!n] matMult([[int,m],n] x, [[int,n],m] y) =
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
both be of either type ``int`` or ``real``.  The following operators
are supported: ``+``, ``*``, ``-``, ``/``, ``\%``, ``==``, ``<``,
``<=``, ``pow``.

``x`` *bitop* ``y``
~~~~~~~~~~~~~~~~~~~

Evaluate the binary bitwise operator on its operands, which must both
be of type ``int``.  The following operators are supported: ``^``,
``&``, ``|``, ``>>``, ``<<``, i.e., bitwise xor, and, or, and
arithmetic shift right and left.

``x && y``
~~~~~~~~~~

Logical conjunction; both operands must be of type ``bool``.  Not
short-circuiting, as this complicates program transformation.  If
short-circuiting behaviour is desired, the programmer can use ``if``
explicitly.

``x || y``
~~~~~~~~~~

Logical disjunction; both operands must be of type ``bool``.  As with
``&&``, not short-circuiting.

``not x``
~~~~~~~~~

Logical negation of ``x``, which must be of type ``bool``.

``- x``
~~~~~~~

Numerical negation of ``x``, which must be of type ``real`` or
``int``.

``a[i]``
~~~~~~~~

Return the element at the given position in the array.  The index may
be a comma-separated list of indexes instead of just a single index.
If the number of indices given is less than the rank of the array, an
array is returned.

``zip(x, y, z)``
~~~~~~~~~~~~~~~~~~

Zips together the elements of the outer dimensions of arrays ``x``,
``y``, and ``z``.  Static or runtime check is performed to check that
the sizes of the outermost dimension of the arrays are the same.  If
this property is not true, program execution stops with an error.  Any
number of arrays may be passed to ``unzip``.  If *n* arrays are given,
the result will be a single-dimensional array of *n*-tuples (where the
the tuple components may themselves be arrays).

``unzip(a)``
~~~~~~~~~~~~

If the type of ``a`` is ``[{t_1, ..., t_n}]``, the result is a tuple
of *n* arrays, i.e., ``{[t_1], ..., [t_n]}``, and otherwise a type
error.

``iota(n)``
~~~~~~~~~~~

An array of the integers from ``0`` to ``n-1``.

``replicate(n, x)``
~~~~~~~~~~~~~~~~~~~

An array consisting of ``n`` copies of ``a``.

``size(i, a)``
~~~~~~~~~~~~~~

The size of dimension ``i`` of array ``a``, where ``i`` is a static
integer constant.

``split((i_1, ..., i_n), a)``
~~~~~~~~~~~~~~~~~

Partitions the given array ``a`` into ``n+1`` disjoint arrays
``{a[0...i_1-1], a[i_1...i_2-1], ..., a[i_n...]}``, returned as a tuple.
The split indices must be weakly ascending, ie ``i_1 <= i_2 <= ... <= i_n``.

Example: ``split((1,1,3), [5,6,7,8]) == {[5],[],[6,7],[8]}``

``concat(a_1, ..., a_n)``
~~~~~~~~~~~~~~~~~~~~~~~~~

Concatenate the rows/elements of several arrays.  The shape of the
arrays must be identical in all but the first dimension.

``copy(a)``
~~~~~~~~~~~
Return a deep copy of the argument.  Semantically, this is just
the identity function, but it has special semantics related to
uniqueness types as described in :ref:`uniqueness-types`.

``reshape((d_1, ..., d_n), a)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Reshape the elements of ``a`` into an ``n``-dimensional array of the
specified shape.  The number of elements in ``a`` must be equal to the
product of the new dimensions.

``rearrange((d_1, ..., d_n), a)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Permute the dimensions in the array, returning a new array.

For example, if ``b==rearrange((2,0,1),a)``, then ``b[x,y,z] =
a[y,z,x]``.

``transpose(k, n, a)``
~~~~~~~~~~~~~~~~~~~~~~

Return the generalised transpose of \textit{a}.  If
``b==transpose(k,n,a)``, then

::

    a[i_1, ..., i_k, i_(k+1), ..., i_(k+n), ..., i_q ]
      =
    b[i_1 , ..., i_(k+1) , ..., i_(k+n), i_k, ..., i_q ]

We will call this an operation an *(k,n)-transposition*.  Note that
``transpose(0,1,a)`` is the common two-dimensional transpose.

Be aware that ``k`` and ``n`` must be static integer literals, and
``k+n`` must be non-negative and smaller than the rank of ``a``, or it
is considered a type error.

This operation is merely syntactical sugar for the equivalent
``rearrange`` operation.

``transpose(a)``
~~~~~~~~~~~~~~~~

Return the transpose of ``a``.  Syntactical sugar for
``transpose(0,1,a)``, which is again syntactical sugar for
``rearrange``.


``let pat = e in body``
~~~~~~~~~~~~~~~~~~~~~~~

Evaluate ``e`` and bind the result to the pattern ``pat`` while
evaluating ``body``.

``let dest = src with [i] <- v in body``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Evaluate ``body`` with ``dest`` bound to the value of
``src``, except that the element(s) at the position given by the
index take on the value of ``v``.  The given index need not be
complete, but in that case, the value of ``v`` must be an array
of the proper size.

``if c then a else b``
~~~~~~~~~~~~~~~~~~~~~~

If ``c`` evaluates to ``True``, evaluate ``a``, else evaluate ``b``.

``loop (pat = initial) = for i < bound do loopbody in body``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The name ``i`` is bound here and initialised to zero.

1. Bind ``pat`` to the initial values given in ``initial``.

2. While ``i < bound``, evaluate ``loopbody``, rebinding ``pat`` to be the
      value returned by the body, increasing ``i`` by one after each
      iteration.

3. Evaluate ``body`` with ``pat`` bound to its final
      value.

``loop (pat = initial) = while cond do loopbody in body``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. Bind ``pat`` to the initial values given in ``initial``.

2. While ``cond`` evaluates to true, evaluate ``loopbody``, rebinding
      ``pat`` to be the value returned by the body.

3. Evaluate ``body`` with ``pat`` bound to its final
      value.

Parallel Expressions
--------------------

It is not guaranteed that the the parallel constructs in Futhark are
evaluated in parallel, especially if they are nested in complicated
ways.  Their purpose is to give the compiler as much freedom and
information is possible, in order to enable it to maximise the
parallelism of the generated code.

``map(f, a)``
~~~~~~~~~~~~~

Apply ``f`` to every element of ``a`` and return the resulting array.

``zipWith(f, a_1, ..., a_n)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Syntactic sugar for ``map(f, zip(a_1, ..., a_n))``.

``reduce(f, x, a)``
~~~~~~~~~~~~~~~~~~~

Left-reduction with ``f`` across the elements of ``a``, with ``x`` as
the neutral element for ``f``.  The function ``f`` must be
associative.  If it is not, the evaluation result is not defined.

``scan(f, x, a)``
~~~~~~~~~~~~~~~~~~~

Inclusive prefix scan.  Has the same caveats with respect to
associativity as ``reduce``.

``filter(f, a)``
~~~~~~~~~~~~~~~~

Remove all those elements of ``a`` that do not satisfy the predicate
``f``.

``partition(f_1, ..., f_n, a)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Divide the array ``a`` into disjoint partitions based on the given
predicates.  Each element of ``a`` is called with the predicates
``f_1`` to ``f_n`` in sequence, and as soon as one as one of them
returns ``True``, the element is added to the corresponding partition.
If none of the functions return ``True``, the element is added to a
catch-all partition that is returned last.  Always returns a tuple
with *n+1* components.  The partitioning is stable, meaning that
elements of the partitions retain their original relative positions.

Tuple Shimming
--------------

In a SOAC, if the given function expects *n* arguments of types
``t_1=, ..., t_n``, but the SOAC will call the function with a
single argument of type ``{t_1, ..., t_n}`` (that is,
a tuple), the Futhark compiler will automatically generate an anonymous
unwrapping function.  This allows the following expression to
type-check (and run)::

  map(+, zip(as, bs))

Without the tuple shimming, the above would cause an error, as ``+``
is a function that takes two arguments, but is passed a two-element
tuple by ``map``.

Arrays of Tuples
----------------

For reasons related to code generation and efficient representation,
arrays of tuples are in a sense merely syntactic sugar for tuples of
arrays.  The type ``[{int, real}]`` is transformed to ``{[int],
[real]}`` during the compilation process, and all code interacting
with arrays of tuples is likewise transformed.  In most cases, this is
fully transparent to the programmer, but there are edge cases where
the transformation is not trivially an isomorphism.

Consider the type ``[{[int], [real]}]``, which is transformed
into ``{[[int]], [[real]]}``.  These two types are not
isomorphic, as the latter has more stringent demands as to the
fullness of arrays.  For example::

  [
    {[1],   [1.0]},
    {[2,3], [2.0]}
  ]

is a value of the former, but the first element of the
corresponding transformed tuple::

  {
    [[1],   [2, 3]],
    [[1.0], [2.0]]
  }

is not a full array.  Hence, when determining whether a program
generates full arrays, we must hence look at the *transformed*
values - in a sense, the fullness requirement "transcends" the tuples.
