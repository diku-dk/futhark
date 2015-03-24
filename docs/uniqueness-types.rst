.. _uniqueness-types:

Uniqueness Types
================

While Futhark is through and through a pure functional language, it
may occasionally prove useful to express certain algorithms in an
imperative style.  Consider a function for computing the *n* first
Fibonacci numbers::

  fun [int] fib(int n) =
    // Create "empty" array.
    let arr = iota(n) in
    // Fill array with Fibonacci numbers.
    loop (arr) = for i < n-2 do
                   let arr[i+2] = arr[i] + arr[i+1]
                   in arr
    in arr

If the array ``arr`` is copied for each iteration of the loop, we
are going to put enormous pressure on memory, and spend a lot of time
moving around data, even though it is clear in this case that the
"old" value of ``arr`` will never be used again.  Precisely,
what should be an algorithm with complexity *O(n)* becomes *(n^2)*
due to copying the size *n* array (an *O(n)* operation) for each of
the *n* iterations of the loop.

To prevent this, we will want to update the array \textit{in-place},
that is, with a static guarantee that the operation will not require
any additional memory allocation, such as copying the array.  With an
in-place modification, a \texttt{let-with} can modify the array in
time proportional to the slice being updated ($O(1)$ in the case of
the Fibonacci function), rather than time proportional to the size of
the final array, as would the case if we perform a copy.  In order to
perform the update without violating referential transparency, we need
to know that no other references to the array exists, or at least that
such references will not be used on any execution path following the
in-place update.

In Futhark, this is done through a type system feature called
*uniqueness types*, similar to, although simpler, than the uniqueness
types of Clean.  Alongside a (relatively) simple aliasing analysis in
the type checker, this is sufficient to determine at compile time
whether an in-place modification is safe, and signal a compile time
error if ``let-with`` is used in way where safety cannot be
guaranteed.

The simplest way to introduce uniqueness types is through examples.
To that end, let us consider the following function definition::

  fun *[int] modify(*[int] a, int i, int x) =
    let b = a with [i] <- a[i] + x in
    b

The function call ``modify(a,i,x)`` returns ``a``, but where the
element at index ``i`` has been increased by ``x``.  Note the
asterisks: in the parameter declaration ``*[int] a``, this means that
the function ``modify`` has been given "ownership" of the array ``a``,
meaning that not caller of ``modify`` will reference array ``a`` after
the call.  In particular, ``modify`` can change the element at index
``i`` without first copying the array, i.e. ``modify`` is free to do
an in-place modification.  Furthermore, the return value of ``modify``
is also unique - this means that the result of the call to ``modify``
does not share elements with any other visible variables.

Let us consider a call to ``modify``, which might look as
follows::

  let b = modify(a, i, x) in
  ..

Under which circumstances is this call valid?  Two things must hold:

1. The type of ``a`` must be ``*[int]``, of course.

2. Neither ``a`` or any variable that *aliases* `a` may be used on any
   execution path following the call to ``modify``.

In general, when a value is passed as a unique-typed argument in a
function call, we consider that value to be *consumed*, and neither it
nor any of its aliases can be used again.  Otherwise, we would break
the contract that gives the function liberty to manipulate the
argument however it wants.  Note that it is the type in the argument
declaration that must be unique - it is permissible to pass a
unique-typed variable as a non-unique argument (that is, a unique type
is a subtype of the corresponding nonunique type).

A variable *v* aliases *a* if they may share some elements,
i.e. overlap in memory.  As the most trivial case, after evaluating
the binding ``let b = a``, the variable ``b`` will alias
``a``.  As another example, if we extract a row from a
two-dimensional array, the row will alias its source::

  let b = a[0] in
  ... // b is aliased to a (assuming a is not one-dimensional)

:ref:`futhark-sharing` will cover sharing and sharing analysis in
     greater detail.

Let us consider the definition of a function returning a unique array::

  fun *[int] f([int] a) = body

Note that the argument, ``a``, is non-unique, and hence we cannot
modify it.  There is another restriction as well: ``a`` must not be
aliased to our return value, as the uniqueness contract requires us to
ensure that there are no other references to the unique return value.
This requirement would be violated if we permitted the return value in
a unique-returning function to alias its non-unique parameters.

To summarise: *values are consumed by being the source in a
``let-with``, or by being passed as a unique parameter in a function
call*.  We can crystallise valid usage in the form of three principal
rules:

  **Uniqueness Rule 1**

    When a value is passed in the place of a unique parameter in a
    function call, or used as the source in a ``let-with`` expression,
    neither that value, nor any value that aliases it, may be used on
    any execution path following the function call.  An example
    violation::

      let b = a with [i] <- 2 in
      f(b,a) // \emp{Error:} a used after being source in a let-with


  **Uniqueness Rule 2**

    If a function definition is declared to return a unique value, the
    return value (that is, the result of the body of the function)
    must not share memory with any non-unique arguments to the
    function.  As a consequence, at the time of execution, the result
    of a call to the function is the only reference to that value.  An
    example violation::

      fun *[int] broken([[int]] a, int i) =
        a[i] // Return value aliased with 'a'.

  **Uniqueness Rule 3**

    If a function call yields a unique return value, the caller has
    exclusive access to that value.  At *the point the call returns*,
    the return value may not share memory with any variable used in
    any execution path following the function call.  This rule is
    particularly subtle, but can be considered a rephrasing of
    Uniqueness Rule 2 from the "calling side".

It is worth emphasising that everything in this chapter is employed as
part of a static analysis.  *All* violations of the uniqueness rules
will be discovered at compile time (in fact, during type-checking),
thus leaving the code generator and runtime system at liberty to
exploit them for low-level optimisation.

.. _futhark-sharing:

Sharing Analysis
----------------

Whenever the memory regions for two values overlap, we say that they
are *aliased*, or that *sharing* is present.  As an example, if you
have a two-dimensional array ``a`` and extract its first row as the
one-dimensional array ``b``, we say that ``a`` and ``b`` are aliased.
While the Futhark compiler may do a deep copy if it wishes, it is not
required, and this operation thus holds the potential for sharing
memory.  Sharing analysis is necessarily conservative, and merely
imposes an upper bound on the amount of sharing happening at runtime.
The sharing analysis in Futhark has been carefully designed to make
the bound as tight as possible, but still easily computable.

In Futhark, the only values that can have any sharing are arrays -
everything else is considered "primitive".  Tuples are special, in
that they are not considered to have any identity beyond their
elements.  Therefore, when we store sharing information for a
tuple-typed expression, we do it for each of its element types, rather
than the tuple value as a whole.

Many operations that produce arrays alias their array-typed inputs.
For example, the result of ``concat`` aliases the arrays being
concatenated.  This may seem counter-intuitive, but gives the compiler
greater freedom.  The programmer can use ``copy`` to "break" sharing
by forcing the argument to be manifested uniquely in memory.
