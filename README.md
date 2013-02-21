L0Language
==========

The L-zero Language.

Installation
==========

Just run `cabal install` and an executable by the name of `l0c` will be
installed in your Cabal bin directory, most likely $HOME/.cabal/bin.

Otherwise, just run `cabal configure`, followed by `cabal build`, and
the executable can be found in `dist/build/l0c/l0c`.

L0 semantics
============

For now, just look at the example programs in the `data/` directory.

Do-loops
--------

The following semantics were based on an idea by Cosmin and
implemented by Troels.  They are pretty much what is currently in the
compiler.

The semantics of a do-loop is:

    for i < N do
        Exp
    merge (v1,..,vn)

where the result of `Exp` is of the same type with that of `(v1,..,vn)` is
equivalent to

        doloopfun( N, 0, (v1,..,vn) )
    where
        doloopfun( i, N, (v1,..,vn) ) =
           if(i = N) then (v1,..,vn)
           else doloopfun(i+1, N, Exp)

`v1...vn` must be in lexical scope at the loop.  Inside the body of the
loop, they are known as _merge variables_, and have special
restrictions.

The semantics of the let-with construct:

    let x1 = x0 with [ind] <- exp_elem in exp_res

is that `x1` is a deep copy of `x0` but with the element at `index
ind` updated to `exp_elem`.  In the above construct, we say that `x1`
is used as _destination_, and `x0` is used as _source_.  If x1 and x0
are the same name, we say that the let-with is _reflexive_. Let-withs
where the destination is a merge variable are intended to be in-place.

If used without a good understanding what the (current) compiler does
for you, let-with may introduce severe overhead.  The following rules
apply:

1. For every expression `let x0 = e in body`, where `e` contains a
merge variable that is used as destination in a reflexive let-with in
body, `e` must yield a _basic type_.  A basic type is a type that is
not an array, and if a tuple, one that contains only basic types.

2. Merge variables defined outside of an anonymous function are out of
scope in the body of the function.

Tuple arguments
---------------

In a SOAC, when the given function is to be invoked with N arguments
of types t1...tN, but the function only takes a single argument of
type (t1 * ... * tN) (that is, a tuple), L0 will automatically
generate an anonymous unwrapping function in such a way that it still
works.  This allows the following expression to type-check (and run):

    map(op +, zip(as, bs))

Without the above transformation, you would get an error, as 'op +' is
a function that takes two arguments, but is passed a tuple by map.

This eliminates the need for a dedicated zipWith construct.
