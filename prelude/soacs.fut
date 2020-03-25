-- | Various Second-Order Array Combinators that are operationally
-- parallel in a way that can be exploited by the compiler.
--
-- The functions here are all recognised specially by the compiler (or
-- built on those that are).  The asymptotic [work and
-- span](https://en.wikipedia.org/wiki/Analysis_of_parallel_algorithms)
-- is provided for each function, but note that this easily hides very
-- substantial constant factors.  For example, `scan`@term is *much*
-- slower than `reduce`@term, although they have the same asymptotic
-- complexity.
--
-- *Reminder on terminology*: A function `op` is said to be
-- *associative* if
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
-- **Work:** *O(n)*
--
-- **Span:** *O(1)*
let map 'a [n] 'x (f: a -> x) (as: [n]a): *[n]x =
  intrinsics.map (f, as) :> *[n]x

-- | Apply the given function to each element of a single array.
--
-- **Work:** *O(n)*
--
-- **Span:** *O(1)*
let map1 'a [n] 'x (f: a -> x) (as: [n]a): *[n]x =
  map f as

-- | As `map1`@term, but with one more array.
--
-- **Work:** *O(n)*
--
-- **Span:** *O(1)*
let map2 'a 'b [n] 'x (f: a -> b -> x) (as: [n]a) (bs: [n]b): *[n]x =
  map (\(a, b) -> f a b) (zip2 as bs)

-- | As `map2`@term, but with one more array.
--
-- **Work:** *O(n)*
--
-- **Span:** *O(1)*
let map3 'a 'b 'c [n] 'x (f: a -> b -> c -> x) (as: [n]a) (bs: [n]b) (cs: [n]c): *[n]x =
  map (\(a, b, c) -> f a b c) (zip3 as bs cs)

-- | As `map3`@term, but with one more array.
--
-- **Work:** *O(n)*
--
-- **Span:** *O(1)*
let map4 'a 'b 'c 'd [n] 'x (f: a -> b -> c -> d -> x) (as: [n]a) (bs: [n]b) (cs: [n]c) (ds: [n]d): *[n]x =
  map (\(a, b, c, d) -> f a b c d) (zip4 as bs cs ds)

-- | As `map4`@term, but with one more array.
--
-- **Work:** *O(n)*
--
-- **Span:** *O(1)*
let map5 'a 'b 'c 'd 'e [n] 'x (f: a -> b -> c -> d -> e -> x) (as: [n]a) (bs: [n]b) (cs: [n]c) (ds: [n]d) (es: [n]e): *[n]x =
  map (\(a, b, c, d, e) -> f a b c d e) (zip5 as bs cs ds es)

-- | Reduce the array `as` with `op`, with `ne` as the neutral
-- element for `op`.  The function `op` must be associative.  If
-- it is not, the return value is unspecified.  If the value returned
-- by the operator is an array, it must have the exact same size as
-- the neutral element, and that must again have the same size as the
-- elements of the input array.
--
-- **Work:** *O(n)*
--
-- **Span:** *O(log(n))*
let reduce [n] 'a (op: a -> a -> a) (ne: a) (as: [n]a): a =
  intrinsics.reduce (op, ne, as)

-- | As `reduce`, but the operator must also be commutative.  This is
-- potentially faster than `reduce`.  For simple built-in operators,
-- like addition, the compiler already knows that the operator is
-- commutative, so plain `reduce`@term will work just as well.
--
-- **Work:** *O(n)*
--
-- **Span:** *O(log(n))*
let reduce_comm [n] 'a (op: a -> a -> a) (ne: a) (as: [n]a): a =
  intrinsics.reduce_comm (op, ne, as)

-- | `reduce_by_index dest f ne is as` returns `dest`, but with each
-- element given by the indices of `is` updated by applying `f` to the
-- current value in `dest` and the corresponding value in `as`.  The
-- `ne` value must be a neutral element for `op`.  If `is` has
-- duplicates, `f` may be applied multiple times, and hence must be
-- associative and commutative.  Out-of-bounds indices in `is` are
-- ignored.
--
-- **Work:** *O(n)*
--
-- **Span:** *O(n)* in the worst case (all updates to same position),
-- but *O(1)* in the best case.
--
-- In practice, the *O(n)* behaviour only occurs if *m* is also very
-- large.
let reduce_by_index 'a [m] [n] (dest : *[m]a) (f : a -> a -> a) (ne : a) (is : [n]i32) (as : [n]a) : *[m]a =
  intrinsics.hist (1, dest, f, ne, is, as) :> *[m]a

-- | Inclusive prefix scan.  Has the same caveats with respect to
-- associativity as `reduce`.
--
-- **Work:** *O(n)*
--
-- **Span:** *O(log(n))*
let scan [n] 'a (op: a -> a -> a) (ne: a) (as: [n]a): *[n]a =
  intrinsics.scan (op, ne, as) :> *[n]a

-- | Remove all those elements of `as` that do not satisfy the
-- predicate `p`.
--
-- **Work:** *O(n)*
--
-- **Span:** *O(log(n))*
let filter [n] 'a (p: a -> bool) (as: [n]a): *[]a =
  let (as', is) = intrinsics.partition (1, \x -> if p x then 0 else 1, as)
  in as'[:is[0]]

-- | Split an array into those elements that satisfy the given
-- predicate, and those that do not.
--
-- **Work:** *O(n)*
--
-- **Span:** *O(log(n))*
let partition [n] 'a (p: a -> bool) (as: [n]a): ([]a, []a) =
  let p' x = if p x then 0 else 1
  let (as', is) = intrinsics.partition (2, p', as)
  in (as'[0:is[0]], as'[is[0]:n])

-- | Split an array by two predicates, producing three arrays.
--
-- **Work:** *O(n)*
--
-- **Span:** *O(log(n))*
let partition2 [n] 'a (p1: a -> bool) (p2: a -> bool) (as: [n]a): ([]a, []a, []a) =
  let p' x = if p1 x then 0 else if p2 x then 1 else 2
  let (as', is) = intrinsics.partition (3, p', as)
  in (as'[0:is[0]], as'[is[0]:is[0]+is[1]], as'[is[0]+is[1]:n])

-- | `reduce_stream op f as` splits `as` into chunks, applies `f` to each
-- of these in parallel, and uses `op` (which must be associative) to
-- combine the per-chunk results into a final result.  The `i32`
-- passed to `f` is the size of the chunk.  This SOAC is useful when
-- `f` can be given a particularly work-efficient sequential
-- implementation.  Operationally, we can imagine that `as` is divided
-- among as many threads as necessary to saturate the machine, with
-- each thread operating sequentially.
--
-- A chunk may be empty, and `f 0 []` must produce the neutral element for
-- `op`.
--
-- **Work:** *O(n)*
--
-- **Span:** *O(log(n))*
let reduce_stream [n] 'a 'b (op: b -> b -> b) (f: (k: i32) -> [k]a -> b) (as: [n]a): b =
  intrinsics.reduce_stream (op, f, as)

-- | As `reduce_stream`@term, but the chunks do not necessarily
-- correspond to subsequences of the original array (they may be
-- interleaved).
--
-- **Work:** *O(n)*
--
-- **Span:** *O(log(n))*
let reduce_stream_per [n] 'a 'b (op: b -> b -> b) (f: (k: i32) -> [k]a -> b) (as: [n]a): b =
  intrinsics.reduce_stream_per (op, f, as)

-- | Similar to `reduce_stream`@term, except that each chunk must produce
-- an array *of the same size*.  The per-chunk results are
-- concatenated.
--
-- **Work:** *O(n)*
--
-- **Span:** *O(1)*
let map_stream [n] 'a 'b (f: (k: i32) -> [k]a -> [k]b) (as: [n]a): *[n]b =
  intrinsics.map_stream (f, as) :> *[n]b

-- | Similar to `map_stream`@term, but the chunks do not necessarily
-- correspond to subsequences of the original array (they may be
-- interleaved).
--
-- **Work:** *O(n)*
--
-- **Span:** *O(1)*
let map_stream_per [n] 'a 'b (f: (k: i32) -> [k]a -> [k]b) (as: [n]a): *[n]b =
  intrinsics.map_stream_per (f, as) :> *[n]b

-- | Return `true` if the given function returns `true` for all
-- elements in the array.
--
-- **Work:** *O(n)*
--
-- **Span:** *O(log(n))*
let all [n] 'a (f: a -> bool) (as: [n]a): bool =
  reduce (&&) true (map f as)

-- | Return `true` if the given function returns `true` for any
-- elements in the array.
--
-- **Work:** *O(n)*
--
-- **Span:** *O(log(n))*
let any [n] 'a (f: a -> bool) (as: [n]a): bool =
  reduce (||) false (map f as)

-- | The `scatter as is vs` expression calculates the equivalent of
-- this imperative code:
--
-- ```
-- for index in 0..length is-1:
--   i = is[index]
--   v = vs[index]
--   as[i] = v
-- ```
--
-- The `is` and `vs` arrays must have the same outer size.  `scatter`
-- acts in-place and consumes the `as` array, returning a new array
-- that has the same type and elements as `as`, except for the indices
-- in `is`.  If `is` contains duplicates (i.e. several writes are
-- performed to the same location), the result is unspecified.  It is
-- not guaranteed that one of the duplicate writes will complete
-- atomically - they may be interleaved.  See `reduce_by_index`@term
-- for a function that can handle this case deterministically.
--
-- This is technically not a second-order operation, but it is defined
-- here because it is closely related to the SOACs.
--
-- **Work:** *O(n)*
--
-- **Span:** *O(1)*
let scatter 't [m] [n] (dest: *[m]t) (is: [n]i32) (vs: [n]t): *[m]t =
  intrinsics.scatter (dest, is, vs) :> *[m]t
