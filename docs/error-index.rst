.. _error-index:

Error Index
===========

Elaboration on type errors produced by the compiler.  Many error
messages contain links to the sections below.

Uniqueness errors
-----------------

.. _use-after-consume:

"Using *x*, but this was consumed at *y*."
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A core principle of uniqueness typing (see :ref:`in-place-updates`) is
that after a variable is "consumed", it must not be used again.  For
example, this is invalid, and will result in the error above::

  let y = x with [0] = 0
  in x

Several operations can *consume* a variable: array update expressions,
calling a function with unique-typed parameters, or passing it as the
initial value of a unique-typed loop parameter.  When a variable is
consumed, its *aliases* are also considered consumed.  Aliasing is the
possibility of two variables occupying the same memory at run-time.
For example, this will fail as above, because ``y`` and ``x`` are
aliased::

  let y = x
  let z = y with [0] = 0
  in x

We can always break aliasing by using a ``copy`` expression::

  let y = copy x
  let z = y with [0] = 0
  in x

.. _not-consumable:

"Would consume *x*, which is not consumable"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This error message occurs for programs that try to perform a
consumption (such as an in-place update) on variables that are not
consumable.  For example, it would occur for the following program::

  let f (a: []i32) =
    let a[0] = a[0]+1
    in a

Only arrays with a a *unique array type* can be consumed.  Such a type
is written by prefixing the array type with an asterisk.  The program
could be fixed by writing it like this::

  let f (a: *[]i32) =
    let a[0] = a[0]+1
    in a

Note that this places extra obligations on the caller of the ``f``
function, since it now *consumes* its argument.  See
:ref:`in-place-updates` for the full details.

You can always obtain a unique copy of an array by using
``copy``::

  let f (a: []i32) =
    let a = copy a
    let a[0] = a[0]+1
    in a

But note that in most cases (although not all), this subverts the
purpose of using in-place updates in the first place.

.. _return-aliased:

"Unique-typed return value of *x* is aliased to *y*, which is not consumable"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This can be caused by a function like this::

  let f (xs: []i32) : *[]i32 = xs

We are saying that ``f`` returns a *unique* array - meaning it has no
aliases - but at the same time, it aliases the parameter *xs*, which
is not marked as being unique (see :ref:`in-place-updates`).  This
violates one of the core guarantees provided by uniqueness types,
namely that a unique return value does not alias any value that might
be used in the future.  Imagine if this was permitted, and we had a
program that used ``f``::

  let b = f a
  let b[0] = x
  ...

The update of ``b`` is fine, but if ``b`` was allowed to alias ``a``
(hence occupying the same memory), then we would be modifying ``a`` as
well, which is a violation of referential transparency.

As with most uniqueness errors, it can be fixed by using ``copy xs``
to break the aliasing.  We can also change the type of ``f`` to take a
unique array as input::

  let f (xs: *[]i32) : *[]i32 = xs

This makes ``xs`` "consumable", in the sense used by the error message.

.. _unique-return-aliased:

"A unique-typed component of the return value of *x* is aliased to some other component"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Caused by programs like the following::

  let main (xs: *[]i32) : (*[]i32, *[]i32) = (xs, xs)

While we are allowed to "consume" ``xs``, as it is a unique parameter,
this function is trying to return two unique values that alias each
other.  This violates one of the core guarantees provided by
uniqueness types, namely that a unique return value does not alias any
value that might be used in the future (see :ref:`in-place-updates`) -
and in this case, the two values alias each other.  We can fix this by
inserting copies to break the aliasing::

  let main (xs: *[]i32) : (*[]i32, *[]i32) = (xs, copy xs)

Size errors
-----------

.. _unused-size:

"Size *x* unused in pattern."
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Caused by expressions like this::

  let [n] (y: i32) = x

And functions like this::

  let f [n] (x: i32) = x

Since ``n`` is not the size of anything, it cannot be assigned a value
at runtime.  Hence this program is rejected.

.. _causality-check:

"Causality check"
~~~~~~~~~~~~~~~~~

Causality check errors occur when the program is written in such a way
that a size is needed before it is actually computed.  See
:ref:`causality` for the full rules.  Contrived example::

  let f (b: bool) (xs: []i32) =
    let a = [] : [][]i32
    let b = [filter (>0) xs]
    in a[0] == b[0]

Here the inner size of the array ``a`` must be the same as the inner
size of ``b``, but the inner size of ``b`` depends on a ``filter``
operation that is executed after ``a`` is constructed.

There are various ways to fix causality errors.  In the above case, we
could merely change the order of statements, such that ``b`` is bound
first, meaning that the size is available by the time ``a`` is bound.
In many other cases, we can lift out the "size-producing" expressions
into a separate ``let``-binding preceding the problematic expressions.
