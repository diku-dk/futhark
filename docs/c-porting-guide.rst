.. _c-porting-guide:

C Porting Guide
===============

This short document contains a collection of tips and tricks for
porting simple numerical C code to futhark.  Futhark's sequential
fragment is powerful enough to permit a rather straightforward
translation of sequential C code that does not rely on pointer
mutation.  Additionally, we provide hints on how to recognise C coding
patterns that are symptoms of C's weak type system, and how better to
organise it in Futhark.

One intended audience of this document is a programmer who needs to
translate a benchmark application written in C, or needs to use a
simple numerical algorithm that is already available in the form of C
source code.

Where This Guide Falls Short
----------------------------

Some C code makes use of unstructured returns and nonlocal exits
(``return`` inside loops, for example).  These are not easy to express
in Futhark, and will require massaging the control flow a bit.  C code
that uses ``goto`` is likewise not easy to port.

Types
-----

Futhark provides scalar types that match the ones commonly used in C:
``u8``/``u16``/``u32``/``u64`` for the unsigned integers,
``i8``/``i16``/``i32``/``i64`` for the signed, and ``f32``/``f64`` for
``float`` and ``double`` respectively.  In contrast to C, Futhark does
not automatically promote types in expressions - you will have to
manually make sure that both operands to e.g. a multiplication are of
the exact same type.  This means that you will need to understand
exactly which types a given expression in original C program operates
on, which generally boils down to converting the type of the
(type-wise) smaller operand to that of the larger.  Note that the
Futhark ``bool`` type is not considered a number.

Operators
---------

Most of the C operators can be found in Futhark with their usual
names.  Note however that the Futhark ``/`` and ``%`` operators for
integers round towards negative infinity, whereas their counterparts
in C round towards zero.  You can write ``//`` and ``%%`` if you want
the C behaviour.  There is no difference if both operands are
non-zero, but ``//`` and ``%%`` may be slightly faster.  For unsigned
numbers, they are exactly the same.

Variable Mutation
-----------------

As a sequential language, most C programs quite obviously rely heavily
on mutating variables.  However, in many programs, this is done in a
static manner without indirection through pointers (except for arrays;
see below), which is conceptually similar to just declaring a new
variable of the same name that shadows the old one.  If this is the
case, a C assignment can generally be translated to just a
``let``-binding.  As an example, let us consider the following
function for computing the modular multiplicative inverse of a 16-bit
unsigned integer (part of the IDEA encryption algorithm):

.. code-block:: c

  static uint16_t ideaInv(uint16_t a) {
    uint32_t b;
    uint32_t q;
    uint32_t r;
    int32_t t;
    int32_t u;
    int32_t v;

    b = 0x10001;
    u = 0;
    v = 1;

    while(a > 0)
      {
        q = b / a;
        r = b % a;

        b = a;
        a = r;

        t = v;
        v = u - q * v;
        u = t;
      }

    if(u < 0)
      u += 0x10001;

    return u;
  }

Each iteration of the loop mutates the variables ``a``, ``b``, ``v``,
and ``u`` in ways that are visible to the following iteration.
Conversely, the "mutations" of ``q``, ``r``, and ``t`` are not truly
mutations, and the variable declarations could be moved inside the
loop if we wished.  Presumably, the C programmer left them outside for
aesthetic reasons.  When translating such code, it is important to
determine exactly how much *true* mutation is going on, and how much
is just reuse of variable space.  This can usually be done by checking
whether a variable is read before it is written in any given
iteration - if not, then it is not true mutation.  The variables that
change value from one iteration of the loop to the next will need to
be maintained as *merge parameters* of the Futhark ``do``-loop.

The Futhark program resulting from a straightforward port looks as
follows:

.. code-block:: futhark

  let main(a: u16): u16 =
    let b = 0x10001u32
    let u = 0i32
    let v = 1i32
    loop ((a,b,u,v)) = while a > 0u16 do
      let q = b / u32(a)
      let r = b % u32(a)

      let b = u32(a)
      let a = u16(r)

      let t = v
      let v = u - i32(q) * v
      let u = t
      in (a,b,u,v)

    in u16(if u < 0 then u + 0x10001 else u)

Note the heavy use of type conversion and type suffixes for constants.
This is necessary due to Futhark's lack of implicit conversions.  Note
also the conspicuous way in which the ``do``-loop is written - the
result of a loop iteration consists of variables whose names are
identical to those of the merge parameters.

This program can still be massaged to make it more idiomatic Futhark -
for example, the variable ``t`` only serves to store the old value of
``v`` that is otherwise clobbered.  This can be written more elegantly
by simply inlining the expressions in the result part of the loop
body.

Arrays
------

Dynamically sized multidimensional arrays are somewhat awkward in C,
and are often simulated via single-dimensional arrays with explicitly
calculated indices:

.. code:: c

  a[i * M + j] = foo;

This indicates a two-dimensional array ``a`` whose *inner* dimension
is of size ``M``.  We can usually look at where ``a`` is allocated to
figure out what the size of the outer dimension must be:

.. code:: c

  a = malloc(N * M * sizeof(int));

We see clearly that ``a`` is a two-dimensional integer array of size
``N`` times ``M`` - or of type ``[N][M]i32`` in Futhark.  Thus, the update
expression above would be translated as::

  let a[i,j] = foo in
  ...

C programs usually first allocate an array, then enter a loop to
provide its initial values.  This is not possible in Futhark -
consider whether you can write it as a ``replicate``, an ``iota``, or
a ``map``.  In the worst case, use ``replicate`` to obtain an array of
the desired size, then use a ``do``-loop with in-place updates to
initialise it (but note that this will run stricly sequentially).
