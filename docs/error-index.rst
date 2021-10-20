.. _error-index:

Compiler Error Index
====================

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

.. _consuming-parameter:

"Consuming parameter passed non-unique argument"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Caused by programs like the following::

  let update (xs: *[]i32) = xs with [0] = 0

  let f (ys: []i32) = update ys

The update ``function`` *consumes* its ``xs`` argument to perform an
:ref:`in-place update <in-place-updates>`, as denoted by the asterisk
before the type.  However, the ``f`` function tries to pass an array
that it is not allowed to consume (no asterisk before the type).


One solution is to change the type of ``f`` so that it also consumes
its input, which allows it to pass it on to ``update``::

  let f (ys: *[]i32) = update ys

Another solution to ``copy`` the array that we pass to ``update``::

  let f (ys: []i32) = update (copy ys)

.. _consuming-argument:

"Non-consuming higher-order parameter passed consuming argument."
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This error occurs when we have a higher-order function that expects a
function that does *not* consume its arguments, and we pass it one
that does::

  let apply 'a 'b (f: a -> b) (x: a) = f x

  let consume (xs: *[]i32) = xs with [0] = 0

  let f (arr: *[]i32) = apply consume arr

We can fix this by changing ``consume`` so that it does not have to
consume its argument, by adding a ``copy``::

  let consume (xs: []i32) = copy xs with [0] = 0

Or we can create a variant of ``apply`` that accepts a consuming
function::

  let apply 'a 'b (f: *a -> b) (x: *a) = f x

.. _alias-free-variable:

"Function result aliases the free variable *x*"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Caused by definitions such as the following::

  let x = [1,2,3]

  let f () = x

To simplify the tracking of aliases, the Futhark type system requires
that the result of a function may only alias the function parameters,
not any free variables.  Use ``copy`` to fix this::

  let f () = copy x

.. _inaccessible-size:

"Parameter *x* refers to size *y* which will not be accessible to the caller
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This happens when the size of an array parameter depends on a name
that cannot be expressed in the function type::

  let f (x: i64, y: i64) (A: [x]bool) = true

Intuitively, this function might have the following type::

  val f : (x: i64, y: i64) -> [x]bool -> bool

But this is not currently a valid Futhark type.  In a function type,
each parameter can be named *as a whole*, but it cannot be taken apart
in a pattern.  In this case, we could fix it by splitting the tuple
parameter into two separate parameters::

  let f (x: i64) (y: i64) (A: [x]bool) = true

This gives the following type::

  val f : (x: i64) -> (y: i64) -> [x]bool -> bool

Another workaround is to loosen the static safety, and use a size
coercion to give A its expected size::

  let f (x: i64, y: i64) (A_unsized: []bool) =
    let A = A_unsized :> [x]bool
    in true

This will produce a function with the following type::

  val f [d] : (i64, i64) -> [d]bool -> bool

This does however lose the constraint that the size of the array must
match one of the elements of the tuple, which means the program may
fail at run-time.

The error is not always due to an explicit type annotation.  It might
also be due to size inference::

  let f (x: i64, y: i64) (A: []bool) = zip A (iota x)

Here the type rules force ``A`` to have size ``x``, leading to a
problematic type.  It can be fixed using the techniques above.

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

.. _unknowable-param-def:

"Unknowable size *x* in parameter of *y*"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This error occurs when you define a function that can never be
applied, as it requires an input of a specific size, and that size is
not known.  Somewhat contrived example::

  let f (x: bool) =
    let n = if x then 10 else 20
    in \(y: [n]bool) -> ...

The above constructs a function that accepts an array of size 10 or
20, based on the value of ``x`` argument.  But the type of ``f true``
by itself would be ``?[n].[n]bool -> bool``, where the ``n`` is
unknown.  There is no way to construct an array of the right size, so
the type checker rejects this program. (In a fully dependently typed
language, the type would have been ``[10]bool -> bool``, but Futhark
does not do any type-level computation.)

In most cases, this error means you have done something you didn't
actually mean to.  However, in the case that that the above really is
what you intend, the workaround is to make the function fully
polymorphic, and then perform a size coercion to the desired size
inside the function body itself::

  let f (x: bool) =
    let n = if x then 10 else 20
    in \(y_any: []bool) ->
         let y = y_any :> [n]bool
         in true

This requires a check at run-time, but it is the only way to
accomplish this in Futhark.

.. _existential-param-ret:

"Existential size would appear in function parameter of return type"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This occurs most commonly when we use function composition with one or
more functions that return an *existential size*.  Example::

  filter (>0) >-> length

The ``filter`` function has this type::

  val filter [n] 't : (t -> bool) -> [n]t -> ?[m].[m]t

That is, ``filter`` returns an array whose size is not known until the
function actually returns.  The ``length`` function has this type::

  val length [n] 't : [n]t -> i64

Whenever ``length`` occurs (as in the composition above), the type
checker must *instantiate* the ``[n]`` with the concrete symbolic size
of its input array.  But in the composition, that size does not
actually exist until ``filter`` has been run.  For that matter, the
type checker does not know what ``>->`` does, and for all it knows it
may actually apply ``filter`` many times to different arrays, yielding
different sizes.  This makes it impossible to uniquely instantiate the
type of ``length``, and therefore the program is rejected.

The common workaround is to use *pipelining* instead of composition
whenever we use functions with existential return types::

  xs |> filter (>0) |> length

This works because ``|>`` is left-associative, and hence the ``xs |>
filter (>0)`` part will be fully evaluated to a concrete array before
``length`` is reached.

We can of course also write it as ``length (filter (>0) xs)``, with no
use of either pipelining or composition.

Module errors
-------------

.. _nested-entry:

"Entry points may not be declared inside modules."
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This occurs when the program uses the ``entry`` keyword inside a
module::

  module m = {
    entry f x = x + 1
  }

Entry points can only be declared at the top level of a file.  When we
wish to make a function from inside a module available as an entry
point, we must define a wrapper function::

  module m = {
    let f x = x + 1
  }

  entry f = m.f

.. _module-is-parametric:

"Module *x* is a parametric module
----------------------------------

A parametric module is a module-level function::

  module PM (P: {val x : i64}) = {
    let y = x + 2
  }

If we directly try to access the component of ``PM``, as ``PM.y``, we
will get an error.  To use ``PM`` we must first apply it to a module
of the expected type::

  module M = PM { val x = 2 : i64 }

Now we can say ``M.y``.  See :ref:`module-system` for more.

Other errors
------------

.. _literal-out-of-bounds:

"Literal out of bounds"
~~~~~~~~~~~~~~~~~~~~~~~

This occurs for overloaded constants such as ``1234`` that are
inferred by context to have a type that is too narrow for their value.
Example::

  257 : u8

It is not an error to have a *non-overloaded* numeric constant whose
value is too large for its type.  The following is perfectly
cromulent::

  257u8

In such cases, the behaviour is overflow (so this is equivalent to
``1u8``).

.. _ambiguous-type:

"Type is ambiguous"
~~~~~~~~~~~~~~~~~~~

There are various cases where the type checker is unable to infer the
full type of something.  For example::

  let f r = r.x

We know that ``r`` must be a record with a field called ``x``, but
maybe the record could also have other fields as well.  Instead of
assuming a perhaps too narrow type, the type checker signals an error.
The solution is always to add a type annotation in one or more places
to disambiguate the type::

  let f (r: {x:bool, y:i32}) = r.x

Usually the best spot to add such an annotation is on a function
parameter, as above.  But for ambiguous sum types, we often have to
put it on the return type.  Consider::

  let f (x: bool) = #some x

The type of this function is ambiguous, because the type checker must
know what other possible contructors (apart from ``#some``) are
possible.  We fix it with a type annotation on the return type::

  let f (x: bool) : (#some bool | #none) = #just x

See :ref:`typeabbrevs` for how to avoid typing long types in several
places.

.. _may-not-be-redefined:

"The *x* operator may not be redefined"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``&&`` and ``||`` operators have magical short-circuiting
behaviour, and therefore may not be redefined.  There is no way to
define your own short-circuiting operators.

.. _unmatched-cases:

"Unmatched cases in match expression"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Futhark requires ``match`` expressions to be *exhaustive* - that is,
cover all possible forms of the value being pattern-matches.
Example::

  let f (x: i32) =
    match x case 0 -> false
            case 1 -> true

Usually this is an actual bug, and you fix it by adding the missing
cases.  But sometimes you *know* that the missing cases will never
actually occur at run-time.  To satisfy the type checker, you can turn
the final case into a wildcard that matches anything::

  let f (x: i32) =
    match x case 0 -> false
            case _ -> true

Alternatively, you can add a wildcard case that explicitly asserts
that it should never happen::

  let f (x: i32) =
    match x case 0 -> false
            case 1 -> true
            case _ -> assert false false

:ref:`See here <assert>` for details on how to use ``assert``.

.. _record-type-not-known:

"Full type of *x* is not known at this point"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When performing a :ref:`record update <record_update>`, the type of the
field we are updating must be known.  This restriction is based on a
limitation in the type type checker, so the notion of "known" is a bit
subtle::

  let f r : {x:i32} = r with x = 0

Even though the return type annotation disambiguates the type, this
program still fails to type check.  This is because the return type is
not consulted until *after* the body of the function has been checked.
The solution is to put a type annotation on the parameter instead::

  let f (r : {x:i32}) = r with x = 0
