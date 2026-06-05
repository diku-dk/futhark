-- | Definitions related to automatic differentiation.
--
-- Futhark supports a fairly complete facility for computing
-- derivatives of functions through automatic differentiation (AD).
-- The following does not constitute a full introduction to AD, which
-- is a substantial topic by itself, but rather introduces enough
-- concepts to describe Futhark's functional AD interface. AD is a
-- program transformation that is implemented in the compiler itself,
-- but the user interface is a handful of fairly simple functions.
--
-- AD is useful when optimising cost functions, and for any other
-- purpose where you might need derivatives, such as for example
-- computing surface normals for signed distance functions.
--
-- Futhark's AD support includes the following:
--
--   * Differentiation operators for forward-mode (`jvp`@term) and reverse-mode
--     (`vjp`@term).
--
--   * Arbitrary control flow in differentiable code.
--
--   * Higher order derivatives by nesting differentiation operators, including
--     arbitrary mixing of forward- and reverse mode (although using multiple
--     rounds of reverse mode is rarely useful and often slow).
--
--   * Custom derivatives (`with_vjp`@term).
--
--   * Vectorised AD (`vjp_vec`@term, `vjp_vec`@term).
--
--   * Checkpointing of sequential loops.
--
-- ## Jacobians
--
-- For a differentiable function *f* whose input comprise *n* scalars
-- and whose output comprises *m* scalars, the
-- [Jacobian](https://en.wikipedia.org/wiki/Jacobian_matrix_and_determinant)
-- for a given input point is an *m* by *n* matrix of scalars that
-- each represent a [partial
-- derivatives](https://en.wikipedia.org/wiki/Partial_derivative).
-- Intuitively, position *(i,j)* of the Jacobian describes how
-- sensitive output *i* is to input *j*. The notion of Jacobian
-- generalises to functions that accept or produce compound structures
-- such as arrays, records, sums, and so on, simply by "flattening
-- out" the values and considering only their constituent scalars.
--
-- Computing the full Jacobian is usually costly and sometimes not
-- necessary, and it is not part of the AD facility provided by
-- Futhark. Instead it is possible to parts of the Jacobian.
--
-- We can take the product of an an *m* by *n* Jacobian with an
-- *n*-element *tangent vector* to produce an *m*-element vector
-- (*Jacobian-vector product*). Such a product can be computed in a
-- single (augmented) execution of the function *f*, and by choosing
-- the tangent vector appropriately we can use this to compute the
-- full Jacobian. This is provided by the function `jvp`.
--
-- We can also take the product of an *m*-element vector *cotangent
-- vector* with the *m* by *n* Jacobian to produce an *n*-element
-- vector (*Vector-Jacobian product*). This too can be computed in a
-- single execution of *f*, with `vjp`.
--
-- We can use the `jvp` function to produce a *column* of the full
-- Jacobian, and `vjp` to produce a *row*. Which is superior for a
-- given situation depends on whether the function has more inputs or
-- outputs.
--
-- We can freely nest `vjp` and `jvp` to compute higher-order derivatives.
--
-- ## Efficiency
--
-- Both `jvp` and `vjp` work by transforming the program to carry
-- along extra information associated with each scalar value.
--
-- In the case of `jvp`, this extra information takes the form of an
-- additional scalar representing the tangent, which is then
-- propagated in each scalar computation using essentially the [chain
-- rule](https://en.wikipedia.org/wiki/Chain_rule). Therefore, `jvp`
-- has a memory overhead of approximately *2x*, and a computational
-- overhead of slightly more, but usually less than *4x*.
--
-- In the case of `vjp`, since our starting point is a *cotangent*,
-- the function is essentially first run forward, then backwards (the
-- *return sweep*) to propagate the cotangent. During the return
-- sweep, all intermediate results computed during the forward sweep
-- must still be available, and must therefore be stored in memory
-- during the forward sweep. This means that the memory usage of `vjp`
-- is proportional to the number of sequential steps of the original
-- function (essentially turning *time* into *space*). The compiler
-- does a nontrivial amount of optimisation to ameliorate this
-- overhead (see [AD for an Array Language with Nested
-- Parallelism](https://futhark-lang.org/publications/sc22-ad.pdf)),
-- but it can still be substantial for programs with deep sequential
-- loops.
--
-- It varies on a case-by-case basis whether vectorised AD is faster or not. It
-- essentially converts propagation of (co-)tangents from scalar to array
-- operations, which can have a significant impact on memory accesses, depending
-- on how the compiler manages to optimise the resulting code. It is hard to
-- predict whether this offsets the reduction in primal work. If the vector size
-- is a constant, and the `#[unroll]` attribute is put on the AD operator, then
-- the vectors become unrolled (turned into tuples, essentially), although this
-- should only be done when the vector size is quite small, as the increase in
-- code size is substantial.
--
-- ## Differentiable functions
--
-- AD only gives meaningful results for differentiable functions. The Futhark
-- type system does not distinguish differentiable from non-differentiable
-- operations. As a rule of thumb, a function is differentiable if its results
-- are computed using a composition of primitive floating-point operations,
-- without ever converting to or from integers.
--
-- Note that a function whose input or output is a sum type with more than one
-- constructor is *not* differentiable (or at least the sum-typed part is not).
-- This is because the choice of constructor is not a continuous quantity.
--
-- ## Limitations
--
-- `jvp` is expected to work in all cases. `vjp` has limitations when using the
-- GPU backends similar to those for irregular flattening. Specifically, you
-- should avoid structures with variant sizes, such as loops that carry an array
-- that changes size through the execution of the loop.

-- | Jacobian-Vector Product ("forward mode"), producing also the
-- primal result as the first element of the result tuple.
def jvp2 'a 'b (f: a -> b) (x: a) (x': a) : (b, b) =
  intrinsics.jvp2 f x x'

-- | Vector-Jacobian Product ("reverse mode"), producing also the
-- primal result as the first element of the result tuple.
def vjp2 'a 'b (f: a -> b) (x: a) (y': b) : (b, a) =
  intrinsics.vjp2 f x y'

-- | As `jvp2`, but accepts a vector of seed values. Semantically equivalent to
-- mapping, but may be more efficient. If used with `#[unroll]`, tangent
-- calculations are unrolled when possible.
def jvp2_vec 'a 'b [n] (f: a -> b) (x: a) (x': [n]a) : (b, [n]b) =
  intrinsics.jvp2_vec f x x'

-- | As `vjp2`, but accepts a vector of seed values. Semantically equivalent to
-- mapping, but may be more efficient. If used with `#[unroll]`, adjoint
-- calculations are unrolled when possible.
def vjp2_vec 'a 'b [n] (f: a -> b) (x: a) (y': [n]b) : (b, [n]a) =
  intrinsics.vjp2_vec f x y'

-- | Jacobian-Vector Product ("forward mode").
def jvp 'a 'b (f: a -> b) (x: a) (x': a) : b =
  (jvp2 f x x').1

-- | Vector-Jacobian Product ("reverse mode").
def vjp 'a 'b (f: a -> b) (x: a) (y': b) : a =
  (vjp2 f x y').1

-- | As `jvp`, but accepts a vector of seed values. Semantically
-- equivalent to mapping, but may be more efficient.
def jvp_vec 'a 'b [n] (f: a -> b) (x: a) (x': [n]a) : [n]b =
  (jvp2_vec f x x').1

-- | As `vjp`, but accepts a vector of seed values. Semantically
-- equivalent to mapping, but may be more efficient.
def vjp_vec 'a 'b [n] (f: a -> b) (x: a) (y': [n]b) : [n]a =
  (vjp2_vec f x y').1

-- | Provide custom reverse-mode adjoint code for a given function. This is
-- useful when the adjoint synthesised by AD is not as good as one that is known
-- analytically.
--
-- The function `f` returns a result of type `b`. In the return sweep, the
-- function `f'` is invoked first with the result of `f` and second with the
-- cotangents of the result (be careful not to mix up the order), and must
-- return the sensitivity with respect to the input.
--
-- A common pattern is that `b` is a tuple where some part is the intended
-- primal result of `with_vjp`, and some part is only used in `f'`.
--
-- **Beware:** if `f` uses any free variables, these will not be taken into
-- **account when computing the adjoint. Make these part of the argument
-- **instead.
def with_vjp 'a 'b (f: a -> b) (f': (res: b) -> (b_adj: b) -> a) (x: a) : b =
  intrinsics.with_vjp f f' x

-- | A variant of `with_vjp` where the intermediate result necessary for the
-- adjoint (`c`) is explicitly separated from the primal result (`b`).
def with_vjp_tape 'a 'b 'c (f: a -> (c, b)) (f': (c, b) -> a) (x: a) : b =
  (with_vjp f (\(tape, _) (_, adj) -> f' (tape, adj)) x).1
