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

   In-place updates

     A somewhat misleading term for the syntactic forms ``x with [i] =
     v`` and ``let x[i] = v``.  These are not semantic in-place
     updates, but can be operationally understood as thus.  See
     :ref:`in-place-updates`.

   Parametric module

     A function from :term:`modules<module>` to modules.  The most
     powerful form of abstraction provided by Futhark.

   Module

     A mapping from names to definitions of types, values, or nested
     modules.  See :ref:`module-system`.

   Size expression

     An expression that occurs as the size of an array or size
     argument.  For example, in the type ``[x+2]i32``, ``x+2`` is a
     size expression.  Size expressions can occur syntactically in
     source code, or due to parameter substitution when applying a
     function.

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
