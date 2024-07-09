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
example, this is invalid, and will result in the error above:

.. code-block:: futhark

  let y = x with [0] = 0
  in x

Several operations can *consume* a variable: array update expressions,
calling a function with unique-typed parameters, or passing it as the
initial value of a unique-typed loop parameter.  When a variable is
consumed, its *aliases* are also considered consumed.  Aliasing is the
possibility of two variables occupying the same memory at run-time.
For example, this will fail as above, because ``y`` and ``x`` are
aliased:

.. code-block:: futhark

  let y = x
  let z = y with [0] = 0
  in x

We can always break aliasing by using a ``copy`` expression:

.. code-block:: futhark

  let y = copy x
  let z = y with [0] = 0
  in x

.. _not-consumable:

"Would consume *x*, which is not consumable"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This error message occurs for programs that try to perform a
consumption (such as an in-place update) on variables that are not
consumable.  For example, it would occur for the following program:

.. code-block:: futhark

  def f (a: []i32) =
    let a[0] = a[0]+1
    in a

Only arrays with a a *unique array type* can be consumed.  Such a type
is written by prefixing the array type with an asterisk.  The program
could be fixed by writing it like this:

.. code-block:: futhark

  def f (a: *[]i32) =
    let a[0] = a[0]+1
    in a

Note that this places extra obligations on the caller of the ``f``
function, since it now *consumes* its argument.  See
:ref:`in-place-updates` for the full details.

You can always obtain a unique copy of an array by using
``copy``:

.. code-block:: futhark

  def f (a: []i32) =
    let a = copy a
    let a[0] = a[0]+1
    in a

But note that in most cases (although not all), this subverts the
purpose of using in-place updates in the first place.

.. _return-aliased:

"Unique-typed return value of *x* is aliased to *y*, which is not consumable"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This can be caused by a function like this:

.. code-block:: futhark

  def f (xs: []i32) : *[]i32 = xs

We are saying that ``f`` returns a *unique* array - meaning it has no
aliases - but at the same time, it aliases the parameter *xs*, which
is not marked as being unique (see :ref:`in-place-updates`).  This
violates one of the core guarantees provided by uniqueness types,
namely that a unique return value does not alias any value that might
be used in the future.  Imagine if this was permitted, and we had a
program that used ``f``:

.. code-block:: futhark

  let b = f a
  let b[0] = x
  ...

The update of ``b`` is fine, but if ``b`` was allowed to alias ``a``
(hence occupying the same memory), then we would be modifying ``a`` as
well, which is a violation of referential transparency.

As with most uniqueness errors, it can be fixed by using ``copy xs``
to break the aliasing.  We can also change the type of ``f`` to take a
unique array as input:

.. code-block:: futhark

  def f (xs: *[]i32) : *[]i32 = xs

This makes ``xs`` "consumable", in the sense used by the error message.

.. _unique-return-aliased:

"A unique-typed component of the return value of *x* is aliased to some other component"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Caused by programs like the following:

.. code-block:: futhark

  def main (xs: *[]i32) : (*[]i32, *[]i32) = (xs, xs)

While we are allowed to "consume" ``xs``, as it is a unique parameter,
this function is trying to return two unique values that alias each
other.  This violates one of the core guarantees provided by
uniqueness types, namely that a unique return value does not alias any
value that might be used in the future (see :ref:`in-place-updates`) -
and in this case, the two values alias each other.  We can fix this by
inserting copies to break the aliasing:

.. code-block:: futhark

  def main (xs: *[]i32) : (*[]i32, *[]i32) = (xs, copy xs)

.. _self-aliasing-arg:

"Argument passed for consuming parameter is self-aliased."
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Caused by programs like the following:

.. code-block:: futhark

  def g (t: *([]i64, []i64)) = 0

  def f n =
    let x = iota n
    in g (x,x)

The function ``g`` expects to consume two separate ``[]i64`` arrays,
but ``f`` passes it a tuple containing two references to the same
physical array.  This is not allowed, as ``g`` must be allowed to
assume that components of consuming record- or tuple parameters have
no internal aliases.  We can fix this by inserting copies to break the
aliasing:

.. code-block:: futhark

  def f n =
    let x = iota n
    in g (copy (x,x))

Alternative, we could duplicate the expression producing the array:

.. code-block:: futhark

  def f n =
    g (iota n, iota n))

.. _consuming-parameter:

"Consuming parameter passed non-unique argument"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Caused by programs like the following:

.. code-block:: futhark

  def update (xs: *[]i32) = xs with [0] = 0

  def f (ys: []i32) = update ys

The update ``function`` *consumes* its ``xs`` argument to perform an
:ref:`in-place update <in-place-updates>`, as denoted by the asterisk
before the type.  However, the ``f`` function tries to pass an array
that it is not allowed to consume (no asterisk before the type).

One solution is to change the type of ``f`` so that it also consumes
its input, which allows it to pass it on to ``update``:

.. code-block:: futhark

  def f (ys: *[]i32) = update ys

Another solution to ``copy`` the array that we pass to ``update``:

.. code-block:: futhark

  def f (ys: []i32) = update (copy ys)

.. _consuming-argument:

"Non-consuming higher-order parameter passed consuming argument."
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This error occurs when we have a higher-order function that expects a
function that does *not* consume its arguments, and we pass it one
that does:

.. code-block:: futhark

  def apply 'a 'b (f: a -> b) (x: a) = f x

  def consume (xs: *[]i32) = xs with [0] = 0

  def f (arr: *[]i32) = apply consume arr

We can fix this by changing ``consume`` so that it does not have to
consume its argument, by adding a ``copy``:

.. code-block:: futhark

  def consume (xs: []i32) = copy xs with [0] = 0

Or we can create a variant of ``apply`` that accepts a consuming
function:

.. code-block:: futhark

  def apply 'a 'b (f: *a -> b) (x: *a) = f x

.. _alias-free-variable:

"Function result aliases the free variable *x*"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Caused by definitions such as the following:

.. code-block:: futhark

  def x = [1,2,3]

  def f () = x

To simplify the tracking of aliases, the Futhark type system requires
that the result of a function may only alias the function parameters,
not any free variables.  Use ``copy`` to fix this:

.. code-block:: futhark

  def f () = copy x

.. _size-expression-bind:

"Size expression with binding is replaced by unknown size."
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To illustrate this error, consider the following program

.. code-block:: futhark

   def main (xs: *[]i64) =
     let a = iota (let n = 10 in n+n)
     in ...

Intuitively, the type of ``a`` should be ``[let n = 10 in n+n]i32``,
but this puts a binding into a size expression, which is invalid.
Therefore, the type checker invents an :term:`unknown size`
variable, say ``l``, and assigns ``a`` the type ``[l]i32``.

.. _size-expression-consume:

"Size expression with consumption is replaced by unknown size."
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To illustrate this error, consider the following program

.. code-block:: futhark

   def consume (xs: *[]i64): i64 = xs[0]

   def main (xs: *[]i64) =
     let a = iota (consume xs)
     in ...

Intuitively, the type of ``a`` should be ``[consume ys]i32``, but this
puts a consumption of the array ``ys`` into a size expression, which
is invalid.  Therefore, the type checker invents an :term:`unknown
size` variable, say ``l``, and assigns ``a`` the type ``[l]i32``.

.. _inaccessible-size:

"Parameter *x* refers to size *y* which will not be accessible to the caller
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This happens when the size of an array parameter depends on a name
that cannot be expressed in the function type:

.. code-block:: futhark

  def f (x: i64, y: i64) (A: [x]bool) = true

Intuitively, this function might have the following type:

.. code-block:: futhark

  val f : (x: i64, y: i64) -> [x]bool -> bool

But this is not currently a valid Futhark type.  In a function type,
each parameter can be named *as a whole*, but it cannot be taken apart
in a pattern.  In this case, we could fix it by splitting the tuple
parameter into two separate parameters:

.. code-block:: futhark

  def f (x: i64) (y: i64) (A: [x]bool) = true

This gives the following type:

.. code-block:: futhark

  val f : (x: i64) -> (y: i64) -> [x]bool -> bool

Another workaround is to loosen the static safety, and use a size
coercion to give A its expected size:

.. code-block:: futhark

  def f (x: i64, y: i64) (A_unsized: []bool) =
    let A = A_unsized :> [x]bool
    in true

This will produce a function with the following type:

.. code-block:: futhark

  val f [d] : (i64, i64) -> [d]bool -> bool

This does however lose the constraint that the size of the array must
match one of the elements of the tuple, which means the program may
fail at run-time.

The error is not always due to an explicit type annotation.  It might
also be due to size inference:

.. code-block:: futhark

  def f (x: i64, y: i64) (A: []bool) = zip A (iota x)

Here the type rules force ``A`` to have size ``x``, leading to a
problematic type.  It can be fixed using the techniques above.

Size errors
-----------

.. _unused-size:

"Size *x* unused in pattern."
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Caused by expressions like this:

.. code-block:: futhark

  def [n] (y: i32) = x

And functions like this:

.. code-block:: futhark

  def f [n] (x: i32) = x

Since ``n`` is not the size of anything, it cannot be assigned a value
at runtime.  Hence this program is rejected.

.. _causality-check:

"Causality check"
~~~~~~~~~~~~~~~~~

Causality check errors occur when the program is written in such a way
that a size is needed before it is actually computed.  See
:ref:`causality` for the full rules.  Contrived example:

.. code-block:: futhark

  def f (b: bool) (xs: []i32) =
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

.. _unknown-param-def:

"Unknown size *x* in parameter of *y*"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This error occurs when you define a function that can never be
applied, as it requires an input of a specific size, and that size is
an :term:`unknown size`.  Somewhat contrived example:

.. code-block:: futhark

  def f (x: bool) =
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
inside the function body itself:

.. code-block:: futhark

  def f (x: bool) =
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
more functions that return an *existential size*.  Example:

.. code-block:: futhark

  filter (>0) >-> length

The ``filter`` function has this type:

.. code-block:: futhark

  val filter [n] 't : (t -> bool) -> [n]t -> ?[m].[m]t

That is, ``filter`` returns an array whose size is not known until the
function actually returns.  The ``length`` function has this type:

.. code-block:: futhark

  val length [n] 't : [n]t -> i64

Whenever ``length`` occurs (as in the composition above), the type
checker must *instantiate* the ``[n]`` with the concrete symbolic size
of its input array.  But in the composition, that size does not
actually exist until ``filter`` has been fully applied.  For that
matter, the type checker does not know what ``>->`` does, and for all
it knows it may actually apply ``filter`` many times to different
arrays, yielding different sizes.  This makes it impossible to
uniquely instantiate the type of ``length``, and therefore the program
is rejected.

The common workaround is to use *pipelining* instead of composition
whenever we use functions with existential return types:

.. code-block:: futhark

  xs |> filter (>0) |> length

This works because ``|>`` is left-associative, and hence the ``xs |>
filter (>0)`` part will be fully evaluated to a concrete array before
``length`` is reached.

We can of course also write it as ``length (filter (>0) xs)``, with no
use of either pipelining or composition.

.. _unused-existential:

"Existential size *n* not used as array size"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This error occurs for type expressions that bind an existential size
for which there is no :term:`constructive use`, such as in the
following examples:

.. code-block:: futhark

  ?[n].bool

  ?[n].bool -> [n]bool

When we use existential quantification, we are required to use the
size constructively within its scope, *in particular* it must not be
exclusively as the parameter or return type of a function.

To understand the motivation behind this rule, consider that when we
use an existential quantifier we are saying that there is *some size*.
The size is not known statically, but must be read from some value
(i.e. array) at runtime.  In the first example above, the existential
size ``n`` is not used at all, so the actual value cannot be
determined at runtime.  In the second example, while an array
``[n]bool`` does exist, it is part of a function type, and at runtime
functions are black boxes and don't "carry" the size of their
parameter or result types.

The workaround is to actually use the existential size.  This can be
as simple as adding a *witness array* of type ``[n]()``:

.. code-block:: futhark

  ?[n].([n](),bool)

  ?[n].([n](), bool -> [n]bool)

Such an array will take up no space at runtime.

.. _anonymous-nonconstructive:

"Type abbreviation contains an anonymous size not used constructively as an array size."
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This error occurs for type abbreviations that use anonymous sizes,
such as the following:

.. code-block:: futhark

   type^ t = []bool -> bool

Such an abbreviation is actually shorthand for

.. code-block:: futhark

   type^ t = ?[n].[n]bool -> bool

which is erroneous, but with workarounds, as explained in
:ref:`unused-existential`.

.. _unify-param-existential:

"Parameter *x* used as size would go out of scope."
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This error tends to happen when higher-order functions are used in a
way that causes a size requirement to become impossible to express.
Real programs that encounter this issue tend to be complicated, but to
illustrate the problem, consider the following contrived function:

.. code-block:: futhark

  def f (n: i64) (m: i64) (b: [n][m]bool) = b[0,0]

We have the following type:

.. code-block:: futhark

  val f : (n: i64) -> (m: i64) -> (b: [n][m]bool) -> bool

Now suppose we say:

.. code-block:: futhark

  def g = uncurry f

What should be the type of ``g``?  Intuitively, something like this:

.. code-block:: futhark

  val g : (n: i64, m: i64) -> (b: [n][m]bool) -> bool

But this is *not* expressible in the Futhark type system - and even if
it were, it would not be easy to infer this in general, as it depends
on exactly what ``uncurry`` does, which the type checker does not
know.

As a workaround, we can use explicit type annotation and size
coercions to give ``g`` an acceptable type:

.. code-block:: futhark

  def g [a][b] (n,m) (b: [a][b]bool) = f n m (b :> [n][m]bool)

Another workaround, which is often the right one in cases not as
contrived as above, is to modify ``f`` itself to produce a *witness*
of the constraint, in the form of an array of shape ``[n][m]``:

.. code-block:: futhark

  def f (n: i64) (m: i64) : ([n][m](), [n][m]bool -> bool) =
    (replicate n (replicate m ()), \b -> b[0,0])

Then ``uncurry f`` works just fine and has the following type:

.. code-block:: futhark

  (i64, i64) -> ?[n][m].([n][m](), [n][m]bool -> bool)

Programming with such *explicit size witnesses* is a fairly advanced
technique, but often necessary when writing advanced size-dependent
code.

.. _unify-consuming-param:

"Parameter types *x* and *y* are incompatible regarding consuming their arguments
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This error occurs when you provide a function that *does* consume its
argument in a context that expects a function that *does not* allow a
function that consumes its argument.

As a simple example, consider the following contrived function that
does consume its argument:

.. code-block:: futhark

   def f (xs: *[]f32) : f32 = 0f32

Now we define another function that is merely ``f``, but with a type
annotation that tries to hide the consumption:

.. code-block:: futhark

   def g : []f32 -> f32 = f

Allowing this would permit us to hide the fact that ``f`` consumes its
argument, which would not be sound, so the type checker complains.

.. _ambiguous-size:

"Ambiguous size *x*"
~~~~~~~~~~~~~~~~~~~~

There are various sources for this error, but they all have the same
ultimate cause: the type checker cannot figure out how some symbolic
size name should be resolved to a concrete size.  The simplest
example, although contrived, is probably this:

.. code-block:: futhark

   let [n][m] (xss: [n][m]i64) = []

The type checker can infer that ``n`` should be zero, but how can it
possibly figure out the shape of the (non-existent) rows of the
two-dimensional array?  This can be fixed in many ways, but adding a
type ascription to the array is one of them: ``[] : [0][2]i64``.

Another common case arises when using holes.  For an expression
``length ???``, how would the type checker figure out the intended
size of the array that the hole represents?  Again, this can be solved
with a type ascription: ``length (??? : [10]bool)``.

Finally, ambiguous sizes can also occur for functions that use size
parameters only in "non-witnessing" position, meaning sizes that are
not actually uses as sizes of real arrays.  An example:

.. code-block:: futhark

   def f [n] (g: [n]i64 -> i64) : i64 = n

   def main = f (\xs -> xs[0])

Note that ``f`` is a higher order function, and that the size
parameter ``n`` is only used in the type of the ``g`` function.
Futhark's value model is such that given a value of type ``[n]i64 ->
i64``, we cannot extract an ``n`` from it.  Using a function such as
``f`` is only valid when ``n`` can be inferred from the usage, which
is not the case here.  Again, we can fix it by adding a type
ascription to disambiguate:

.. code-block:: futhark

   def main = f (\(xs:[1]i64) -> xs[0])

Module errors
-------------

.. _module-is-parametric:

"Module *x* is a parametric module
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A parametric module is a module-level function:

.. code-block:: futhark

  module PM (P: {val x : i64}) = {
    def y = x + 2
  }

If we directly try to access the component of ``PM``, as ``PM.y``, we
will get an error.  To use ``PM`` we must first apply it to a module
of the expected type:

.. code-block:: futhark

  module M = PM { val x = 2 : i64 }

Now we can say ``M.y``.  See :ref:`module-system` for more.

Other errors
------------

.. _literal-out-of-bounds:

"Literal out of bounds"
~~~~~~~~~~~~~~~~~~~~~~~

This occurs for overloaded constants such as ``1234`` that are
inferred by context to have a type that is too narrow for their value.
Example:

.. code-block::

  257 : u8

It is not an error to have a *non-overloaded* numeric constant whose
value is too large for its type.  The following is perfectly
cromulent:

.. code-block::

  257u8

In such cases, the behaviour is overflow (so this is equivalent to
``1u8``).

.. _ambiguous-type:

"Type is ambiguous"
~~~~~~~~~~~~~~~~~~~

There are various cases where the type checker is unable to infer the
full type of something.  For example:

.. code-block:: futhark

  def f r = r.x

We know that ``r`` must be a record with a field called ``x``, but
maybe the record could also have other fields as well.  Instead of
assuming a perhaps too narrow type, the type checker signals an error.
The solution is always to add a type annotation in one or more places
to disambiguate the type:

.. code-block:: futhark

  def f (r: {x:bool, y:i32}) = r.x

Usually the best spot to add such an annotation is on a function
parameter, as above.  But for ambiguous sum types, we often have to
put it on the return type.  Consider:

.. code-block:: futhark

  def f (x: bool) = #some x

The type of this function is ambiguous, because the type checker must
know what other possible contructors (apart from ``#some``) are
possible.  We fix it with a type annotation on the return type:

.. code-block:: futhark

  def f (x: bool) : (#some bool | #none) = #just x

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
cover all possible forms of the value being matched.  Example:

.. code-block:: futhark

  def f (x: i32) =
    match x case 0 -> false
            case 1 -> true

Usually this is an actual bug, and you fix it by adding the missing
cases.  But sometimes you *know* that the missing cases will never
actually occur at run-time.  To satisfy the type checker, you can turn
the final case into a wildcard that matches anything:

.. code-block:: futhark

  def f (x: i32) =
    match x case 0 -> false
            case _ -> true

Alternatively, you can add a wildcard case that explicitly asserts
that it should never happen:

.. code-block:: futhark

  def f (x: i32) =
    match x case 0 -> false
            case 1 -> true
            case _ -> assert false false

:ref:`See here <assert>` for details on how to use ``assert``.

.. _refutable-pattern:

"Refutable pattern not allowed here"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This occurs when you try to use a :term:`refutable pattern` in a
``let`` binding or function parameter. A refutable pattern is a
pattern that is not guaranteed to match a well-typed value. For
example, this expression tries to bind an arbitrary tuple value ``x``
a pattern that requires the first element is ``2``:

.. code-block:: futhark

   let (2, y) = x in 0

What should happen at run-time if ``x`` is not 2? Refutable patterns
are only allowed in ``match`` expressions, where the failure to match
can be handled.  For example:

.. code-block:: futhark

   match x
   case (2, y) ->  0
   case _ -> ... -- do something else

.. _record-type-not-known:

"Full type of *x* is not known at this point"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When performing a :ref:`record update <record_update>`, the type of the
field we are updating must be known.  This restriction is based on a
limitation in the type type checker, so the notion of "known" is a bit
subtle:

.. code-block:: futhark

  def f r : {x:i32} = r with x = 0

Even though the return type annotation disambiguates the type, this
program still fails to type check.  This is because the return type is
not consulted until *after* the body of the function has been checked.
The solution is to put a type annotation on the parameter instead:

.. code-block:: futhark

  def f (r : {x:i32}) = r with x = 0

Entry points
------------

.. _nested-entry:

"Entry points may not be declared inside modules."
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This occurs when the program uses the ``entry`` keyword inside a
module:

.. code-block:: futhark

  module m = {
    entry f x = x + 1
  }

Entry points can only be declared at the top level of a file.  When we
wish to make a function from inside a module available as an entry
point, we must define a wrapper function:

.. code-block:: futhark

  module m = {
    def f x = x + 1
  }

  entry f = m.f

.. _polymorphic-entry:

"Entry point functions may not be polymorphic."
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Entry points are Futhark functions that can be called from other
languages, and are therefore limited how advanced their types can be.
In this case, the problem is that an entry point may not have a
polymorphic type, for example:

.. code-block:: futhark

   entry dup 't (x: t) : (t,t) = x

This is an invalid entry point because it uses a type parameter
``'t``.  This error occurs frequently when we want to test a
polymorphic function.  In such cases, the solution is to define one or
more *monomorphic* entry points, each for a distinct type.  For
example, to we can define a variety of monomorphic entry points that
call the built-in function ``scan``:

.. code-block:: futhark

   entry scan_i32 (xs: []i32) = scan (+) 0 xs

   entry scan_f32 (xs: []i32) = scan (*) 1 xs

.. _higher-order-entry:

"Entry point functions may not be higher-order."
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Entry points are Futhark functions that can be called from other
languages, and are therefore limited how advanced their types can be.
In this case, the problem is that an entry point may use functions as
input or output.  For example:

.. code-block:: futhark

   entry apply (f: i32 -> i32) (x: i32) = f x

There is no simple workaround for such cases.  One option is to
manually `defunctionalise
<https://en.wikipedia.org/wiki/Defunctionalization>`_ to use a
non-functional encoding of the functional values, but this can quickly
get very elaborate.  Following up on the example above, if we know
that the only functions that would ever be passed are ``(+y)`` or
``(*y)`` for some ``y``, we could do something like the following:

.. code-block:: futhark

   type function = #add i32 | #mul i32

   entry apply (f: function) (x: i32) =
     match f
     case #add y -> x + y
     case #mul y -> x + y

Although in many cases, the best solution is simply to define a
handful of simpler entry points instead of a single complicated one.

.. _size-polymorphic-entry:

"Entry point functions must not be size-polymorphic in their return type."
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This somewhat rare error occurs when an entry point returns an array
that can have an arbitrary size chosen by its caller.  Contrived example:

.. code-block:: futhark

   -- Entry point taking no parameters.
   entry f [n] : [0][n]i32 = []

The size ``n`` is chosen by the caller.  Note that the ``n`` might be
inferred and invisible, as in this example:

.. code-block:: futhark

   entry g : [0][]i32 = []

When calling functions within a Futhark program, size parameters are
handled by type inference, but entry points are called from the
outside world, which is not subject to type inference.  If you really
must have entry points like this, turn the size parameter into an
ordinary parameter:

.. code-block:: futhark

   entry f (n: i64) : [0][n]i32 = []

.. _nonconstructive-entry:

"Entry point size parameter [n] only used non-constructively."
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This error occurs for programs such as the following::

.. code-block:: futhark

   entry main [x] (A: [x+1]i32) = ...

The size parameter ``[x]`` is only used in an size expression ``x+1``,
rather than directly as an array size.  This is allowed for ordinary
functions, but not for entry points.  The reason is that entry points
are not subject to ordinary type inference, as they are called from
the external world, meaning that the value of the size parameter
``[x]`` will have to be determined from the size of the array ``A``.
This is in principle not a problem for simple sizes like ``x+1``, as
it is obvious that ``x == length A - 1``, but in the general case it
would require computing function inverses that might not exist.  For
this reason, entry points require that all size parameters are used
:term:`constructively<constructive use>`.

As a workaround, you can rewrite the entry point as follows:

.. code-block:: futhark

   entry main [n] (A: [n]i32) =
     let x = n-1
     let A = A :> [x+1]i32
     ...

Or by passing the ``x`` explicitly:

.. code-block:: futhark

   entry main (x: i64) (A: [x+1]i32) = ...
