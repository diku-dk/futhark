.. _glossary:

Glossary
========

The following defines various Futhark-specific terms used in the
documentation and in compiler output.

.. glossary::
   :sorted:

   Abstract type

     A type whose definition has been hidden through a :term:`module
     ascription`.

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
       * ``([n]bool, [n+1]bool)``

     The following do not:

       * ``[n+1]bool``
       * ``bool -> [n]bool``
       * ``[n]bool -> bool``

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

   Defunctionalisation

     A program transformation always performed by the Futhark compiler,
     that replaces function values with non-function values.  The goal
     is to avoid having indirect calls through function pointers at
     run-time.  To permit zero-overhead defunctionalisation, the
     Futhark type rules impose restrictions on :term:`lifted types
     <lifted type>`.

   Defunctorisation

     A program transformation always performed by the Futhark
     compiler, that compiles away modules using an approach similar to
     :term:`defunctionalisation`.  This makes using e.g. a
     :term:`parametric module` completely free at run-time.

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

   Higher-ranked type

     A type that does not describe :term:`values <value>`.  Can be
     seen as a partially applied :term:`type constructor`.  Not
     directly supported by Futhark, but a similar effect can be
     achieved through the :ref:`module-system`.

   In-place updates

     A somewhat misleading term for the syntactic forms ``x with [i] =
     v`` and ``let x[i] = v``.  These are not semantic in-place
     updates, but can be operationally understood as thus.  See
     :ref:`in-place-updates`.

   Invariant

     Not :term:`variant`.

   Irrefutable pattern

     A :term:`pattern` that will always match a value of its type. For
     example, ``(x,y)`` is a pattern that will match any tuple. See
     also :term:`refutable pattern`.

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
     the outer parallelism.  For example, the following expression
     exhibits irregular nested data parallelism::

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

   Polymorphic

     Usually means a :term:`polymorphic function`, but sometimes a
     :term:`parametric modules <parametric module>`.  Should not be
     used to describe a :term:`type constructor <type constructor>`.

   Polymorphism

     The concept of being :term:`polymorphic`.

   Polymorphic function

     A function with :term:`type parameters <type parameter>`, such
     that the function can be applied to arguments of various types.
     Compiled using :term:`monomorphisation`.

   Lifted type

     A type that may contain functions, including function types
     themselves.  These have various restrictions on their use in
     order to support :term:`defunctionalisation`.  See :ref:`hofs`.

   Module

     A mapping from names to definitions of types, values, or nested
     modules.  See :ref:`module-system`.

   Module ascription

     A feature of the module system through which the contents of a
     module can be hidden.  Written as ``m : mt`` where ``m`` is a
     :term:`module expression` and ``mt`` is a :term:`module type
     expression`.  See :ref:`module-system`.

   Module expression

     An expression that is evaluated at compile time, through
     :term:`defunctorisation` to a :term:`module`.  Most commonly just
     the name of a module.

   Module type

     A description of the interface of a :term:`module`.  Most commonly
     used to hide contents in a :term:`module ascription` or to
     require implementation of an interface in a :term:`parametric
     module`.

   Module type expression

     An expression that is evaluated during type-checking to a
     :term:`module type`.

   Monomorphisation

     A program transformation that instantiates a copy of each
     :term:`polymorphic` functions for each type it is used with.
     Performed by the Futhark compiler.

   Name

     A lexical token consisting of alphanumeric characters and
     underscores, for example ``map`` and ``do_it``.  Most variables
     are names.  See also :term:`symbol`.

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

   Pattern

     A syntactical construct for decomposing a value into its
     consituent parts. Patterns are used in function parameters,
     ``let``-bindings, and ``match``. See :ref:`patterns`.

   Recursion

     A function that calls itself.  Currently not supported in
     Futhark.

   Refutable pattern

     A :term:`pattern` that does does not match all possible values.
     For example, the pattern ``(1,x)`` matches only tuples where the
     first element is ``1``. These may not be used in ``let``
     expressions or in function parameters. See also
     :term:`irrefutable pattern`.

   Regular nested data parallelism

     An instance of :term:`nested data parallelism` that is not
     :term:`irregular`.  Fully supports by any :term:`GPU backend`.

   Size

     The symbolic size of an array dimension or :term:`abstract type`.

   Size expression

     An expression that occurs as the size of an array or size
     argument.  For example, in the type ``[x+2]i32``, ``x+2`` is a
     size expression.  Size expressions can occur syntactically in
     source code, or due to parameter substitution when applying a
     :term:`size-dependent function`.

   Size-dependent function

     A function where the size of the result depends on the values of
     the parameters.  The function ``iota`` is perhaps the simplest
     example.

   Size types
   Size-dependent types

     An umbrella term for the part of Futhark's type system that
     tracks array sizes.  See :ref:`size-types`.

   Size-lifted type

     A type that may contain internal hidden sizes.  These cannot be
     array elements, as that might potentially result in an
     :term:`irregular array`.  See :ref:`typeabbrevs`.

   Size argument

     An argument to a :term:`type constructor` in a :term:`type
     expression` of the form ``[n]`` or ``[]``.  The latter is called
     an :term:`anonymous size`.  Must match a corresponding
     :term:`size parameter`.

   Size parameter

     A parameter of a :term:`polymorphic function` or :term:`type
     constructor` that ranges over :term:`sizes <size>`.  These are
     written as `[n]` for some `n`, after which `n` is in scope as a
     term of type ``i64`` within the rest of the definition.  Do not
     confuse them with :term:`type parameters <type parameter>`.

   SOAC
   Second Order Array Combinator

     A term covering the main parallel building blocks provided by
     Futhark: functions such as ``map``, ``reduce``, ``scan``, and so
     on.  They are *second order* because they accept a functional
     argument, and so permit :term:`nested data parallelism`.

   Symbol

     A lexical token that consts of symbolic (non-alphabetic
     characters), and can be bound to a value.  Infix operators such
     as ``+`` and ``/`` are symbols.  See also :term:`name`.

   Type

     A classification of values.  ``i32`` and ``[10]i32`` are examples
     of types.

   Type abbreviation

     A shorthand for a longer type, e.g. ``type t = [100]i32``.  Can
     accept :term:`type parameters <type parameter>` and :term:`size
     parameters <type parameter>`.  The definition is visible to
     users, unless hidden with a :term:`module ascription`.  See
     :ref:`typeabbrevs`.

   Type argument

     An argument to a :term:`type constructor` that is itself a
     :term:`type`.  Must match a corresponding :term:`type parameter`.

   Type constructor

     A :term:`type abbreviation` or :term:`abstract type` that has at
     least one :term:`type parameter` or :term:`size parameter`.
     Futhark does not support :term:`higher-ranked types
     <higher-ranked type>`, so when referencing a type constructor in
     a :term:`type expression`, you must provide corresponding
     :term:`type arguments <type argument>` and :term:`size arguments
     <size argument>` in an appopriate order.

   Type expression

     A syntactic construct that is evaluated to a :term:`type` in the
     type checker, but may contain uses of :term:`type abbreviations
     <type abbreviation>` and :term:`anonymous sizes <anonymous size>`.

   Type parameter

     A parameter of a :term:`polymorphic function` or :term:`type
     constructor` that ranges over types.  These are written as `'t`
     for some `t`, after which `t` is in scope as a type within the
     rest of the definition.  Do not confuse them with :term:`size
     parameters <size parameter>`.

   Uniqueness types

     A somewhat misleading term that describes Futhark's system of
     allowing :term:`consumption` of values, in the interest of
     allowing :term:`in-place updates`.  The only place where
     *uniqueness* truly occurs is in return types, where e.g. the
     return type of ``copy`` is *unique* to indicate that the result
     does not :term:`alias<aliasing>` the argument.

   Unknown size

     A size produced by invoking a function whose result type contains
     an existentially quantified size, such as ``filter``, or because
     the original :term:`size expression` involves variables that have
     gone out of scope.

   Value

     An object such as the integer ``123`` or the array ``[1,2,3]``.
     Expressions variables are bound to values and all valid
     expressions have a :term:`type` describing the form of values
     they can return.

   Variant

     When some value ``v`` computed inside a loop takes a different
     value for each iteration inside the loop, we say that ``v`` is
     *variant* to the loop (and otherwise :term:`invariant`).  Often
     used to talk about :term:`irregularity <irregular>`.  When
     something is nested inside multiple loops, it may be variant to
     just one of them.
