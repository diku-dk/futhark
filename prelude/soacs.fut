-- | Various Second-Order Array Combinators that are operationally
-- parallel in a way that can be exploited by the compiler.
--
-- The functions here are recognised specially by the compiler (or
-- built on those that are).  The asymptotic [work and
-- span](https://en.wikipedia.org/wiki/Analysis_of_parallel_algorithms)
-- is provided for each function, but note that this easily hides very
-- substantial constant factors.  For example, `scan`@term is *much*
-- slower than `reduce`@term, although they have the same asymptotic
-- complexity.
--
-- **Higher-order complexity**
--
-- Specifying the time complexity of higher-order functions is tricky
-- because it depends on the functional argument.  We use the informal
-- convention that *W(f)* denotes the largest (asymptotic) *work* of
-- function *f*, for the values it may be applied to.  Similarly,
-- *S(f)* denotes the largest span.  See [this Wikipedia
-- article](https://en.wikipedia.org/wiki/Analysis_of_parallel_algorithms)
-- for a general introduction to these constructs.
--
-- **Reminder on terminology**
--
-- A function `op` is said to be *associative* if
--
--     (x `op` y) `op` z == x `op` (y `op` z)
--
-- for all `x`, `y`, `z`.  Similarly, it is *commutative* if
--
--     x `op` y == y `op` x
--
-- The value `o` is a *neutral element* if
--
--     x `op` o == o `op` x == x
--
-- for any `x`.

-- Implementation note: many of these definitions contain dynamically
-- checked size casts.  These will be removed by the compiler, but are
-- necessary for the type checker, as the 'intrinsics' functions are
-- not size-dependently typed.

import "zip"

-- | Apply the given function to each element of an array.
--
-- **Work:** *O(n ✕ W(f))*
--
-- **Span:** *O(S(f))*
def map 'a [n] 'x (f: a -> x) (as: [n]a) : *[n]x =
  intrinsics.map f as

-- | Apply the given function to each element of a single array.
--
-- **Work:** *O(n ✕ W(f))*
--
-- **Span:** *O(S(f))*
def map1 'a [n] 'x (f: a -> x) (as: [n]a) : *[n]x =
  map f as

-- | As `map1`@term, but with one more array.
--
-- **Work:** *O(n ✕ W(f))*
--
-- **Span:** *O(S(f))*
def map2 'a 'b [n] 'x (f: a -> b -> x) (as: [n]a) (bs: [n]b) : *[n]x =
  map (\(a, b) -> f a b) (zip2 as bs)

-- | As `map2`@term, but with one more array.
--
-- **Work:** *O(n ✕ W(f))*
--
-- **Span:** *O(S(f))*
def map3 'a 'b 'c [n] 'x (f: a -> b -> c -> x) (as: [n]a) (bs: [n]b) (cs: [n]c) : *[n]x =
  map (\(a, b, c) -> f a b c) (zip3 as bs cs)

-- | As `map3`@term, but with one more array.
--
-- **Work:** *O(n ✕ W(f))*
--
-- **Span:** *O(S(f))*
def map4 'a 'b 'c 'd [n] 'x (f: a -> b -> c -> d -> x) (as: [n]a) (bs: [n]b) (cs: [n]c) (ds: [n]d) : *[n]x =
  map (\(a, b, c, d) -> f a b c d) (zip4 as bs cs ds)

-- | As `map3`@term, but with one more array.
--
-- **Work:** *O(n ✕ W(f))*
--
-- **Span:** *O(S(f))*
def map5 'a 'b 'c 'd 'e [n] 'x (f: a -> b -> c -> d -> e -> x) (as: [n]a) (bs: [n]b) (cs: [n]c) (ds: [n]d) (es: [n]e) : *[n]x =
  map (\(a, b, c, d, e) -> f a b c d e) (zip5 as bs cs ds es)

-- | Reduce the array `as` with `op`, with `ne` as the neutral
-- element for `op`.  The function `op` must be associative.  If
-- it is not, the return value is unspecified.  If the value returned
-- by the operator is an array, it must have the exact same size as
-- the neutral element, and that must again have the same size as the
-- elements of the input array.
--
-- **Work:** *O(n ✕ W(op))*
--
-- **Span:** *O(log(n) ✕ W(op))*
--
-- Note that the complexity implies that parallelism in the combining
-- operator will *not* be exploited.
def reduce [n] 'a (op: a -> a -> a) (ne: a) (as: [n]a) : a =
  intrinsics.reduce op ne as

-- | As `reduce`, but the operator must also be commutative.  This is
-- potentially faster than `reduce`.  For simple built-in operators,
-- like addition, the compiler already knows that the operator is
-- commutative, so plain `reduce`@term will work just as well.
--
-- **Work:** *O(n ✕ W(op))*
--
-- **Span:** *O(log(n) ✕ W(op))*
def reduce_comm [n] 'a (op: a -> a -> a) (ne: a) (as: [n]a) : a =
  intrinsics.reduce_comm op ne as

-- | `h = hist op ne k is as` computes a generalised `k`-bin histogram
-- `h`, such that `h[i]` is the sum of those values `as[j]` for which
-- `is[j]==i`.  The summation is done with `op`, which must be a
-- commutative and associative function with neutral element `ne`.  If
-- a bin has no elements, its value will be `ne`.
--
-- **Work:** *O(k + n ✕ W(op))*
--
-- **Span:** *O(n ✕ W(op))* in the worst case (all updates to same
-- position), but *O(W(op))* in the best case.
--
-- In practice, linear span only occurs if *k* is also very large.
def hist 'a [n] (op: a -> a -> a) (ne: a) (k: i64) (is: [n]i64) (as: [n]a) : *[k]a =
  intrinsics.hist_1d 1 (map (\_ -> ne) (0..1..<k)) op ne is as

-- | Like `hist`, but with initial contents of the histogram, and the
-- complexity is proportional only to the number of input elements,
-- not the total size of the histogram.
--
-- **Work:** *O(n ✕ W(op))*
--
-- **Span:** *O(n ✕ W(op))* in the worst case (all updates to same
-- position), but *O(W(op))* in the best case.
--
-- In practice, linear span only occurs if *k* is also very large.
def reduce_by_index 'a [k] [n] (dest: *[k]a) (f: a -> a -> a) (ne: a) (is: [n]i64) (as: [n]a) : *[k]a =
  intrinsics.hist_1d 1 dest f ne is as

-- | As `reduce_by_index`, but with two-dimensional indexes.
def reduce_by_index_2d 'a [k] [n] [m] (dest: *[k][m]a) (f: a -> a -> a) (ne: a) (is: [n](i64, i64)) (as: [n]a) : *[k][m]a =
  intrinsics.hist_2d 1 dest f ne is as

-- | As `reduce_by_index`, but with three-dimensional indexes.
def reduce_by_index_3d 'a [k] [n] [m] [l] (dest: *[k][m][l]a) (f: a -> a -> a) (ne: a) (is: [n](i64, i64, i64)) (as: [n]a) : *[k][m][l]a =
  intrinsics.hist_3d 1 dest f ne is as

-- | Inclusive prefix scan.  Has the same caveats with respect to
-- associativity and complexity as `reduce`.
--
-- **Work:** *O(n ✕ W(op))*
--
-- **Span:** *O(log(n) ✕ W(op))*
def scan [n] 'a (op: a -> a -> a) (ne: a) (as: [n]a) : *[n]a =
  intrinsics.scan op ne as

-- | Split an array into those elements that satisfy the given
-- predicate, and those that do not.
--
-- **Work:** *O(n ✕ W(p))*
--
-- **Span:** *O(log(n) ✕ W(p))*
def partition [n] 'a (p: a -> bool) (as: [n]a) : ?[k].([k]a, [n - k]a) =
  let p' x = if p x then 0 else 1
  let (as', is) = intrinsics.partition 2 p' as
  in (as'[0:is[0]], as'[is[0]:n])

-- | Split an array by two predicates, producing three arrays.
--
-- **Work:** *O(n ✕ (W(p1) + W(p2)))*
--
-- **Span:** *O(log(n) ✕ (W(p1) + W(p2)))*
def partition2 [n] 'a (p1: a -> bool) (p2: a -> bool) (as: [n]a) : ?[k][l].([k]a, [l]a, [n - k - l]a) =
  let p' x = if p1 x then 0 else if p2 x then 1 else 2
  let (as', is) = intrinsics.partition 3 p' as
  in ( as'[0:is[0]]
     , as'[is[0]:is[0] + is[1]] :> [is[1]]a
     , as'[is[0] + is[1]:n] :> [n - is[0] - is[1]]a
     )

-- | Return `true` if the given function returns `true` for all
-- elements in the array.
--
-- **Work:** *O(n ✕ W(f))*
--
-- **Span:** *O(log(n) + S(f))*
def all [n] 'a (f: a -> bool) (as: [n]a) : bool =
  reduce (&&) true (map f as)

-- | Return `true` if the given function returns `true` for any
-- elements in the array.
--
-- **Work:** *O(n ✕ W(f))*
--
-- **Span:** *O(log(n) + S(f))*
def any [n] 'a (f: a -> bool) (as: [n]a) : bool =
  reduce (||) false (map f as)

local type~ acc 't = intrinsics.acc t

local
def scatter_stream [k] 'a 'b
                   (dest: *[k]a)
                   (f: *acc ([k]a) -> b -> acc ([k]a))
                   (bs: []b) : *[k]a =
  intrinsics.scatter_stream dest f bs :> *[k]a

local
def write [n] 't (acc: *acc ([n]t)) (i: i64) (v: t) : *acc ([n]t) =
  intrinsics.acc_write acc i v

-- | Like `spread`, but takes an array indicating the initial values,
-- and has different work complexity.
--
-- **Work:** *O(n)*
--
-- **Span:** *O(1)*
def scatter 't [k] [n] (dest: *[k]t) (is: [n]i64) (vs: [n]t) : *[k]t =
  scatter_stream dest
                 (\(acc: *acc ([k]t)) (i, v) ->
                    write acc i v)
                 (zip is vs)

-- | `r = spread k x is vs` produces an array `r` such that `r[i] =
-- vs[j]` where `is[j] == i`, or `x` if no such `j` exists.
-- Intuitively, `is` is an array indicating where the corresponding
-- elements of `vs` should be located in the result.  Out-of-bounds
-- elements of `is` are ignored.  In-bounds duplicates in `is` result
-- in unspecified behaviour - see `hist`@term for a function that can
-- handle this.
--
-- **Work:** *O(k + n)*
--
-- **Span:** *O(1)*
def spread 't [n] (k: i64) (x: t) (is: [n]i64) (vs: [n]t) : *[k]t =
  scatter (map (\_ -> x) (0..1..<k)) is vs

-- | `scatter_2d as is vs` is the equivalent of a `scatter` on a 2-dimensional
-- array.
--
-- **Work:** *O(n)*
--
-- **Span:** *O(1)*
def scatter_2d 't [k] [n] [l] (dest: *[k][n]t) (is: [l](i64, i64)) (vs: [l]t) : *[k][n]t =
  intrinsics.scatter_2d dest is vs

-- | `scatter_3d as is vs` is the equivalent of a `scatter` on a 3-dimensional
-- array.
--
-- **Work:** *O(n)*
--
-- **Span:** *O(1)*
def scatter_3d 't [k] [n] [o] [l] (dest: *[k][n][o]t) (is: [l](i64, i64, i64)) (vs: [l]t) : *[k][n][o]t =
  intrinsics.scatter_3d dest is vs

-- | Remove all those elements of `as` that do not satisfy the
-- predicate `p`.
--
-- **Work:** *O(n ✕ W(p))*
--
-- **Span:** *O(log(n) ✕ W(p))*
def filter [n] 'a (p: a -> bool) (as: [n]a) : *[]a =
  let flags = map (\x -> if p x then 1 else 0) as
  let offsets = scan (+) 0 flags
  let m = if n == 0 then 0 else offsets[n - 1]
  in scatter (#[scratch] map (\x -> x) as[:m])
             (map2 (\f o -> if f == 1 then o - 1 else -1) flags offsets)
             as
