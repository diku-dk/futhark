.. _performance:

Writing Fast Futhark Programs
=============================

This document contains tips, tricks, and hints for writing efficient
Futhark code.  Ideally you'd need to know nothing more than an
abstract cost model, but sometimes it is useful to have an idea of how
the compiler will transform your program, what values look like in
memory, and what kind of code the compiler will generate for you.
These details are documented below.  Don't be discouraged by the
complexities mentioned here - most Futhark programs are written
without worrying about any of these details, and they still manage to
run with good performance.  This document focuses on corner cases and
pitfalls, which easily makes for depressing reading.

Parallelism
-----------

The Futhark compiler only generates parallel code for explicitly
parallel constructs such as ``map`` and ``reduce``.  A plain ``loop``
will *not* result in parallel code (unless the loop body itself
contains parallel operations).  The most important parallel constructs
are the *second-order array combinators* (SOACs) such as ``map`` and
``reduce``, but functions such as ``copy`` are also parallel.

When describing the asymptotic cost of a Futhark function, it is not
enough to give a traditional big-O measure of the total amount of
work.  Both ``foldl`` and ``reduce`` involve *O(n)* work, where *n* is
the size of the input array, but ``foldl`` is sequential while
``reduce`` is parallel, and this is an important distinction.  To make
this distinction, each function is described by *two* costs: the
*work*, which is the total amount of operations, and the *span*
(sometimes called *depth*) which is intuitively the "longest chain of
sequential dependencies".  We say that ``foldl`` has span *O(n)*,
while ``reduce`` has span *O(log(n))*.  This explains that
``reduce`` is more parallel than ``foldl``.  The documentation for a
Futhark function should mention both its work and span.  `See this
<https://sigkill.dk/writings/par/cost.html>`_ for more details on
parallel cost models and pointers to literature.

Scans and reductions
~~~~~~~~~~~~~~~~~~~~

The ``scan`` and ``reduce`` SOACs are rather inefficient when their
operators are on arrays.  If possible, use tuples instead (see
:ref:`performance-small-arrays`).  The one exception is when the
operator is a ``map2`` or equivalent.  Example:

.. code-block:: futhark

  reduce (map2 (+)) (replicate n 0) xss

Such "vectorised" operators are typically handled quite efficiently.
Although to be on the safe side, you can rewrite the above by
interchanging the ``reduce`` and ``map``:

.. code-block:: futhark

  map (reduce (+) 0) (transpose xss)

Avoid reductions over tiny arrays, e.g. ``reduce (+) 0 [x,y,z]``.  In
such cases the compiler will generate complex code to exploit a
negligible amount of parallelism.  Instead, just unroll the loop
manually (``x+y+z``) or perhaps use ``foldl (+) 0 [x,z,y]``, which
produces a sequential loop.

Histograms
~~~~~~~~~~

The ``reduce_by_index`` construct ("generalised histogram") has a
clever and adaptive implementation that handles multiple updates of
the same bin efficiently.  Its main weakness is when computing a very
large histogram (many millions of bins) where only a tiny fraction of
the bins are updated.  This is because the main mechanism for
optimising conflicts is by duplicating the histogram in memory, but
this is not efficient when it is very large.  If you know your program
involves such a situation, it may be better to implement the histogram
operation by sorting and then performing an irregular segmented
reduction.

Particularly with the GPU backends, ``reduce_by_index`` is much faster
when the operator involves a single 32-bit or 64-bit value.  Even if
you really want an 8-bit or 16-bit result, it may be faster to compute
it with a 32-bit or 64-bit type and manually mask off the excess bits.

Nested parallelism
~~~~~~~~~~~~~~~~~~

Futhark allows nested parallelism, understood as a parallel construct
used inside some other parallel construct.  The simplest example is
nested SOACs.  Example:

.. code-block:: futhark

  map (\xs -> reduce (+) 0 xs) xss

Nested parallelism is allowed and encouraged, but its compilation to
efficient code is rather complicated, depending on the compiler
backend that is used.  The problem is that sometimes exploiting all
levels of parallelism is not optimal, yet how much to exploit depends
on run-time information that is not available to the compiler.

Sequential backends
!!!!!!!!!!!!!!!!!!!

The sequential backends are straightforward: all parallel operations
are compiled into sequential loops.  Due to Futhark's low-overhead
data representation (see below), this is often surprisingly efficient.

Multicore backend
!!!!!!!!!!!!!!!!!

Whenever the multicore backend encounters nested parallelism, it
generates two code versions: one where the nested parallel constructs
are also parallelised (possibly recursively involving further nested
parallelism), and one where they are turned into sequential loops.  At
runtime, based on the amount of work available and self-tuning
heuristics, the scheduler picks the version that it believes best
balances overhead with exploitation of parallelism.

GPU backends
!!!!!!!!!!!!

The GPU backends handle parallelism through an elaborate program
transformation called *incremental flattening*.  The full details are
beyond the scope of this document, but some properties are useful to
know of.  `See this paper
<https://futhark-lang.org/publications/ppopp19.pdf>`_ for more
details.

The main restriction is that the GPU backends can only handle
*regular* nested parallelism, meaning that the sizes of inner parallel
dimensions are invariant to the outer parallel dimensions.  For
example, this expression contains *irregular* nested parallelism:

.. code-block:: futhark

  map (\i -> reduce (+) 0 (iota i)) is

This is because the size of the nested parallel construct is ``i``,
and ``i`` has a different value for every iteration of the outer
``map``.  The Futhark compiler will currently turn the irregular
constructs (here, the ``reduce``) into a sequential loop.  Depending
on how complicated the irregularity is, it may even refuse to generate
code entirely.

Incremental flattening is based on generating multiple code versions
to cater to different classes of datasets.  At run-time, one of these
versions will be picked for execution by comparing properties of the
input (its size) with a *threshold parameter*.  These threshold
parameters have sensible defaults, but for optimal performance, they
can be tuned with :ref:`futhark-autotune(1)`.

Value Representation
--------------------

The compiler discards all type abstraction when compiling.  Using the
module system to make a type abstract causes no run-time overhead.

Scalars
~~~~~~~

Scalar values (``i32``, ``f64``, ``bool``, etc) are represented as
themselves.  The internal representation does not distinguish signs,
so ``i32`` and ``u32`` have the same representation, and converting
between types that differ only in sign is free.

Tuples
~~~~~~

Tuples are flattened and then represented directly by their individual
components - there are no *tuple objects* at runtime.  A function that
takes an argument of type ``(f64,f64)`` corresponds to a C function
that takes two arguments of type ``double``.  This has one performance
implication: whenever you pass a tuple to a function, the *entire*
tuple is copied (except any embedded arrays, which are always passed
by reference, see below).  Due to the compiler's heavy use of
inlining, this is rarely a problem in practice, but it can be a
concern when using the ``loop`` construct with a large tuple as the
loop variant parameter.

Records
~~~~~~~

Records are turned into tuples by simply sorting their fields and
discarding the labels.  This means there is no overhead to using a
record compared to using a tuple.

Sum Types
~~~~~~~~~

A sum type value is represented as a tuple containing all the payload
components in order, prefixed with an `i8` tag to identify the
constructor.  For example,

.. code-block:: futhark

   #foo i32 bool | #bar i32

would be represented as a tuple of type

.. code-block:: futhark

   (i8, i32, bool, i32)

where the value

.. code-block:: futhark

   #foo 42 false

is represented as

.. code-block:: futhark

   (1, 42, false, 0)

where ``#foo`` is assigned the tag ``1`` because it is alphabetically
after ``#bar``.

To shrink the tuples, if multiple constructors have payload elements
of the *same* type, the compiler assigns them to the same elements in
the result tuple. The representation of the above sum type is actually
the following:

.. code-block:: futhark

   (i8, i32, bool)

The types must be the *same* for deduplication to take place - despite
`i32` and `f32` being of the same size, they cannot be assigned the
same tuple element.  This means that the type

.. code-block:: futhark

   #foo [n]i32 | #bar [n]i32

is efficiently represented as

.. code-block:: futhark

   (u8, [n]i32)

However the type

.. code-block:: futhark

   #foo [n]i32 | #bar [n]f32

is represented as

.. code-block:: futhark

   (u8, [n]i32, [n]f32)

which is not great.  Take caution when you use sum types with large
arrays in their payloads.

Unary sum types
!!!!!!!!!!!!!!!

As an optimisation, the ``i8`` tag is elided when a sum type has only a single
constructor. This means you can always use unary sum types with zero overhead.

Functions
~~~~~~~~~

Higher-order functions are implemented via defunctionalisation.  At
run-time, they are represented by a record containing their lexical
closure.  Since the type system forbids putting functions in arrays,
this is essentially a constant cost, and not worth worrying about.

Arrays
~~~~~~

Arrays are the only Futhark values that are boxed - that is, are
stored on the heap.

The elements of an array are unboxed, stored adjacent to each other in
memory.  There is zero memory overhead except for the minuscule amount
needed to track the shape of the array.

Multidimensional arrays
!!!!!!!!!!!!!!!!!!!!!!!

At the surface language level, Futhark may appear to support "arrays
of arrays", and this is indeed a convenient aspect of its programming
model, but at runtime multi-dimensional arrays are stored in flattened
form.  A value of type ``[x][y]i32`` is laid out in memory simply as
one array containing *x\*y* integers.  This means that constructing an
array ``[x,y,x]`` can be (relatively) expensive if ``x``, ``y``, ``z``
are themselves large arrays, as they must be copied in their entirety.

Since arrays cannot contain other arrays, memory management only has
to be concerned with one level of indirection.  In practice, it means
that Futhark can use straightforward reference counting to keep track
of when to free the memory backing an array, as circular references
are not possible.  Further, since arrays tend to be large and
relatively few in number, the usual performance impact of naive
reference counting is not present.

Arrays of tuples
!!!!!!!!!!!!!!!!

For arrays of tuples, Futhark uses the so-called `structure of arrays
<https://en.wikipedia.org/wiki/AoS_and_SoA>`_ representation.  In
Futhark terms, an array ``[n](a,b,c)`` is at runtime represented as
the tuple ``([n]a,[n]b,[n]c)``.  This means that the final memory
representation always consists of arrays of scalars.

This has some significant implications.  For example, ``zip`` and
``unzip`` are very cheap, as the actual runtime representation is in
always "unzipped", so these functions don't actually have to do
anything.

Since records and sum types are represented as tuples, this also
explains how arrays of these are represented.

Element order
!!!!!!!!!!!!!

The exact in-memory element ordering is up to the compiler, and
depends on how the array is constructed and how it is used.  Absent
any other information, Futhark represents multidimensional arrays in
row-major order.  However, depending on how the array is traversed,
the compiler may insert code to represent it in some other order.  For
particularly tricky programs, an array may even be duplicated in
memory, represented in different ways, to ensure efficient traversal.
This means you should normally *not* worry about how to represent your
arrays to ensure coalesced access on GPUs or similar.  That is the
compiler's job.

Crucial Optimisations
---------------------

Some of the optimisations done by the Futhark compiler are important,
complex, or subtle enough that it may be useful to know how they work,
and how to write code that caters to their quirks.

Fusion
~~~~~~

Futhark performs fusion of SOACs.  For an expression ``map f (map g
A)``, then the compiler will optimise this into a single ``map`` with
the composition of ``f`` and ``g``, which prevents us from storing an
intermediate array in memory.  This is called *vertical fusion* or
*producer-consumer fusion*.  In this case the *producer* is ``map g``
and the *consumer* is ``map f``.

Fusion does not depend on the expressions being adjacent as in this
example, as the optimisation is performed on a data dependency graph
representing the program.  This means that you can decompose your
programs into many small parallel operations without worrying about
the overhead, as the compiler will fuse them together automatically.

Not all producer-consumer relationships between SOACs can be fused.
Generally, ``map`` can always be fused as a producer, but ``scan``,
``reduce``, and similar SOACs can only act as consumers.

*Horizontal fusion* occurs when two SOACs take as input the same
array, but are not themselves in a producer-consumer relationship.
Example:

.. code-block:: futhark

   (map f xs, map g xs)

Such cases are fused into a single operation that traverses ``xs``
just once.  More than two SOACs can be involved in horizontal fusion,
and they need not be of the same kind (e.g. one could be a ``map`` and
the other a ``reduce``).

Free Operations
---------------

Some operations such as array slicing, ``take``, ``drop``,
``transpose`` and ``reverse`` are "free" in the sense that they merely
return a different view of some underlying array.  In most cases they
have constant cost, no matter the size of the array they operate on.
This is because they are index space transformations that simply
result in different code being generated when the arrays are
eventually used.

However, there are some cases where the compiler is forced to manifest
such a "view" as an actual array in memory, which involves a full
copy.  An incomplete list follows:

* Any array returned by an entry point is converted to row-major
  order.

* An array returned by an ``if`` branch may be copied if its
  representation is substantially different from that of the other
  branch.

* An array returned by a ``loop`` body may be copied if its
  representation is substantially different from that of the initial
  loop values.

* An array is copied whenever it becomes the element of another
  multidimensional array.  This is most obviously the case for array
  literals (``[x,y,z]``), but also for ``map`` expressions where the
  mapped function returns an array.

Note that this notion of "views" is not part of the Futhark type
system - it is merely an implementation detail.  Strictly speaking,
all functions that return an array (e.g. ``reverse``) should be
considered to have a cost proportional to the size of the array, even
if that cost will almost never actually be paid at run-time.  If you
want to be sure no copy takes place, it may be better to explicitly
maintain tuples of indexes into some other array.

.. _performance-small-arrays:

Small Arrays
------------

The compiler assumes arrays are "large", which for example means that
operations across them are worth parallelising.  It also means they
are boxed and heap-allocated, even when the size is a small constant.
This can cause unexpectedly bad performance when using small
constant-size arrays (say, five elements or less).  Consider using
tuples or records instead.  `This post
<https://futhark-lang.org/blog/2019-01-13-giving-programmers-what-they-want.html>`_
contains more information on how and why.  If in doubt, try both and
measure which is faster.

Inlining
--------

The compiler currently inlines all functions at their call site,
unless they have been marked with the ``noinline`` attribute (see
:ref:`attributes`).  This can lead to code explosion, which mostly
results in slow compile times, but can also affect run-time
performance.  In many cases this is currently unavoidable, but
sometimes the program can be rewritten such that instead of calling
the same function in multiple places, it is called in a single place,
in a loop.  E.g. we might rewrite ``f x (f y (f z v))`` as:

.. code-block:: futhark

  loop acc = v for a in [z,y,x] do
    f a acc
