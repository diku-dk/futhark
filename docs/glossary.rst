.. _glossary:

Glossary
========

The following defines various Futhark-specific terms used in the
documentation and in compiler output.

.. glossary::
   :sorted:

   Aliases

     The *aliases* of a variable is a set of those other variables
     with which it might be :term:`aliased<aliasing>`.

   Aliasing

     Whether two values might potentially share the same memory at
     run-time.  Clearly, after ``let y = x``, ``x`` and ``y`` are
     aliased.  Also, the slice ``x[i:j]`` is also aliased with ``x``.
     Aliasing is used to type-check :ref:`in-place-updates`.

   Anonymous size

     In a type expression, a size of the form `[]`.  Will be
     :term:`elaborated<elaboration>` to some name (possibly
     :term:`existentially bound<existential size>`) by the type
     checker.

   Attribute

     Auxiliary information attached to an expression or declaration,
     which the compiler or other tool might use for various purposes.
     See :ref:`attributes`.

   Coercion

     Shorthand for a :ref:`size-coercion`.

   Compiler backend

     A Futhark compiler backend is technically only responsible for
     the final compilation result, but from the user's perspective is
     also coupled with a :term:`compiler pipeline`.  The backend
     corresponds to compiler subcommand, such as ``futhark c``,
     ``futhark cuda``, ``futhark multicore``, etc.

   Compiler frontend

     The part of the compiler responsible for reading code from files,
     parsing it, and type checking it.

   Compiler pipeline

     The series of compiler passes that lie between the
     :term:`compiler frontend` and the :term:`compiler backend`.
     Responsible for the majority of program optimisations.  In
     principle the pipeline could be configurable, but in practice
     each backend is coupled with a specific pipeline.

   Constructive use

     A variable ``n`` is used *constructively* in a type if it is used
     as the size of an array at least once outside of any function
     arrows.  For example, the following types use ``n``
     constructively:

       * ``[n]bool``
       * ``([n]bool, bool -> [n]bool)``

     The following do not:

       * ``[n+1]bool``
       * ``bool -> [n]bool``

   Consumption

     If a value is passed for a *consuming* function parameter, that
     value may no longer be used.  We say that a an expression is
     *with consumption* if any values are consumed in the expression.
     This is banned in some cases where that expression might
     otherwise be evaluated multiple times. See
     :ref:`in-place-updates`.

   Data parallelism

     Performing the same operation on multiple elements of a
     collection, such as an array.  The ``map`` :term:`SOAC` is the
     simplest example.  This is the form of parallelism supported by
     Futhark. `See also Wikipedia
     <https://en.wikipedia.org/wiki/Data_parallelism>`_.

   Elaboration

     The process conducted out by the type checker, where it infers
     and inserts information not explicitly provided in the program.
     The most important part of this is type inference, but also
     includes various other things.

   Existential size

     An existential size is a size that is bound by the existential
     quantifier ``?`` in the same type.  For example, in a type
     ``[n]bool -> ?[m].[m]bool``, the size ``m`` is existential.  When
     such a function is applied, each existential size is instantiated
     as an :term:`unknown size`.

   Functor

     The Standard ML term for what Futhark calls a :term:`parametric
     module`.

   GPU backend

     A :term:`compiler backend` that ultimately produces GPU code.
     The backends ``opencl`` and ``gpu`` are GPU backends.  These have
     more restrictions than some other backends, particularly with
     respect to :term:`irregular nested data parallelism`.

   In-place updates

     A somewhat misleading term for the syntactic forms ``x with [i] =
     v`` and ``let x[i] = v``.  These are not semantic in-place
     updates, but can be operationally understood as thus.  See
     :ref:`in-place-updates`.

   Irregular

     Something that is not regular.  Usually used as shorthand for
     :term:`irregular nested data parallelism` or :term:`irregular
     array`.

   Irregular array

     An array where the elements do not have the same size.  For
     example, ``[[1], [2,3]`` is irregular.  These are not supported
     in Futhark.

   Irregular nested data parallelism

     An instance of :term:`nested data parallelism`, where the
     :term:`parallel width` of inner parallelism is :term:`variant` to
     the outer parallelism.  For example, the expression following
     expression exhibits irregular nested data parallelism::

       map (\n -> reduce (+) 0 (iota n)) ns

     Because the width of the inner ``reduce`` is ``n``, and every
     iteration of the outer ``map`` has a (potentially) different
     ``n``.  The Futhark :term:`GPU backends<GPU backend>` *currently*
     do not support irregular nested data parallelism well, and will
     usually sequentialise the irregular loops.  In cases that require
     an :term:`irregular memory allocation`, the compiler may entirely
     fail to generate code.

   Irregular memory allocation

     A situation that occurs when the generated code has to allocate
     memory inside of an instance of :term:`nested data parallelism`,
     where the amount to allocate is variant to the outer parallel
     levels.  As a contrived example (that the actual compiler would
     just optimise away), consider::

       map (\n -> let A = iota n
                  in A[10])
           ns

     To construct the array ``A`` in memory, we require ``8n`` bytes,
     but ``n`` is not known until we start executing the body of the
     ``map``.  While such simple cases are handled, more complicated
     ones that involve nested sequential loops are not supported by
     the :term:`GPU backends<GPU backend>`.

   Parametric module

     A function from :term:`modules<module>` to modules.  The most
     powerful form of abstraction provided by Futhark.

   Lifted type

     A type that may contain functions.  These have various
     restrictions on their use.  See :ref:`hofs`.

   Module

     A mapping from names to definitions of types, values, or nested
     modules.  See :ref:`module-system`.

   Nested data parallelism

     Nested :term:`data parallelism` occurs when a parallel construct
     is used inside of another parallel construct.  For example, a
     ``reduce`` might be used inside a function passed to ``map``.

   Parallel width

     A somewhat informal term used to describe the size of an array on
     which we apply a :term:`SOAC`.  For example, if ``x`` has type
     ``[1000]i32``, then ``map f x`` has a parallel width of 1000.
     Intuitively, the "amount of processors" that would be needed to
     fully exploit the parallelism of the program, although
     :term:`nested data parallelism` muddles the picture.

   Regular nested data parallelism

     An instance of :term:`nested data parallelism` that is not
     :term:`irregular`.  Fully supports by any :term:`GPU backend`.

   Size types
   Size-dependent types

     An umbrella term for the part of Futhark's type system that
     tracks array sizes.  See :ref:`size-types`.

   Size-lifted type

     A type that may contain internal hidden sizes.  These cannot be
     array elements, as that might potentially result in an
     :term:`irregular array`.  See :ref:`typeabbrevs`.

   SOAC
   Second Order Array Combinator

     A term covering the main parallel building blocks provided by
     Futhark: functions such as ``map``, ``reduce``, ``scan``, and so
     on.  They are *second order* because they accept a functional
     argument, and so permit :term:`nested data parallelism`.

   Uniqueness types

     A somewhat misleading term that describes Futhark's system of
     allowing :term:`consumption` of values, in the interest of
     allowing :term:`in-place updates`.  The only place where
     *uniqueness* truly occurs is in return types, where e.g. the
     return type of ``copy`` is *unique* to indicate that the result
     does not :term:`alias<aliasing>` the argument.

   Unknown size

     A size produced by invoking a function whose result type contains
     an existentially quantified size, such as ``filter``.
