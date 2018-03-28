-- | Utility functions for arrays.

import "/futlib/math"

-- | The size of the outer dimension of an array.
let length [n] 't (_: [n]t) = n

-- | Is the array empty?
let null [n] 't (_: [n]t) = n == 0

-- | The first element of the array.
let head [n] 't (x: [n]t) = x[0]

-- | The last element of the array.
let last [n] 't (x: [n]t) = x[n-1]

-- | Everything but the first element of the array.
let tail [n] 't (x: [n]t) = x[1:]

-- | Everything but the last element of the array.
let init [n] 't (x: [n]t) = x[0:n-1]

-- | Take some number of elements from the head of the array.
let take [n] 't (i: i32) (x: [n]t): [i]t = x[0:i]

-- | Remove some number of elements from the head of the array.
let drop [n] 't (i: i32) (x: [n]t) = x[i:]

-- | Split an array at a given position.
let split 't (n: i32) (xs: []t): ([n]t, []t) =
  (xs[:n], xs[n:])

-- | Split an array at two given positions.
let split2 't (i: i32) (j: i32) (xs: []t): ([i]t, []t, []t) =
  (xs[:i], xs[i:j], xs[j:])

-- | Return the elements of the array in reverse order.
let reverse [n] 't (x: [n]t): [n]t = x[::-1]

-- | Replace an element of the array with a new value.
let update [n] 't (xs: *[n]t) (i: i32) (x: t): *[n]t = xs with [i] <- x

-- | Construct an array of consecutive integers of the given length,
-- starting at 0.
let iota (n: i32): *[n]i32 =
  i32.iota n

-- | Construct an array of the given length containing the given
-- value.
let replicate 't (n: i32) (x: t): *[n]t =
  i32.replicate n x

-- | Copy an array.
let copy [n] 't (a: [n]t): *[n]t =
  map (\x -> x) a

-- | Apply the given function to each element of a single array.
let map1 'a [n] 'x (f: a -> x) (as: [n]a): *[n]x =
  map f as

-- | As 'map1', but with one more array.
let map2 'a 'b [n] 'x (f: a -> b -> x) (as: [n]a) (bs: [n]b): *[n]x =
  map (\(a, b) -> f a b) (zip as bs)

-- | As 'map2', but with one more array.
let map3 'a 'b 'c [n] 'x (f: a -> b -> c -> x) (as: [n]a) (bs: [n]b) (cs: [n]c): *[n]x =
  map (\(a, b, c) -> f a b c) (zip as bs cs)

-- | As 'map3', but with one more array.
let map4 'a 'b 'c 'd [n] 'x (f: a -> b -> c -> d -> x) (as: [n]a) (bs: [n]b) (cs: [n]c) (ds: [n]d): *[n]x =
  map (\(a, b, c, d) -> f a b c d) (zip as bs cs ds)

-- | As 'map4', but with one more array.
let map5 'a 'b 'c 'd 'e [n] 'x (f: a -> b -> c -> d -> e -> x) (as: [n]a) (bs: [n]b) (cs: [n]c) (ds: [n]d) (es: [n]e): *[n]x =
  map (\(a, b, c, d, e) -> f a b c d e) (zip as bs cs ds es)

-- | Reduce the array ``as`` with ``op``, with ``ne`` as the neutral
-- element for ``op``.  The function ``op`` must be associative.  If
-- it is not, the return value is unspecified.
let reduce 'a (op: a -> a -> a) (ne: a) (as: []a): a =
  intrinsics.reduce (op, ne, as)

-- | As ``reduce``, but the operator must also be commutative.  This
-- is potentially faster than ``reduce``.  For simple built-in
-- operators, like addition, the compiler already knows that the
-- operator is associative.
let reduce_comm 'a (op: a -> a -> a) (ne: a) (as: []a): a =
  intrinsics.reduce_comm (op, ne, as)

-- | Inclusive prefix scan.  Has the same caveats with respect to
-- associativity as ``reduce``.
let scan [n] 'a (op: a -> a -> a) (ne: a) (as: [n]a): *[n]a =
  intrinsics.scan (op, ne, as)

-- | Remove all those elements of ``as`` that do not satisfy the
-- predicate ``p``.
let filter 'a (p: a -> bool) (as: []a): *[]a =
  let (as', is) = intrinsics.partition (1, \x -> if p x then 0 else 1, as)
  in as'[:is[0]]

-- | Split an array into those elements that satisfy the given
-- predicate, and those that do not.
let partition 'a (p: a -> bool) (as: []a): ([]a, []a) =
  let p' x = if p x then 0 else 1
  let (as', is) = intrinsics.partition (2, p', as)
  in (as'[:is[0]], as'[is[0]:])

-- | Split an array by two predicates, producing three arrays.
let partition2 'a (p1: a -> bool) (p2: a -> bool) (as: []a): ([]a, []a, []a) =
  let p' x = if p1 x then 0 else if p2 x then 1 else 2
  let (as', is) = intrinsics.partition (3, p', as)
  in (as'[:is[0]], as'[is[0]:is[0]+is[1]], as'[is[0]+is[1]:])

-- | The ``scatter as is vs`` expression calculates the equivalent of
-- this imperative code::
--
--   for index in 0..length is-1:
--     i = is[index]
--     v = vs[index]
--     as[i] = v
--
-- The ``is`` and ``vs`` arrays must have the same outer size.  ``scatter``
-- acts in-place and consumes the ``as`` array, returning a new array
-- that has the same type and elements as ``as``, except for the indices
-- in ``is``.  If ``is`` contains duplicates (i.e. several writes are
-- performed to the same location), the result is unspecified.  It is not
-- guaranteed that one of the duplicate writes will complete atomically -
-- they may be interleaved.
let scatter 't [m] [n] (dest: *[m]t) (is: [n]i32) (vs: [n]t): *[m]t =
  intrinsics.scatter (dest, is, vs)

let stream_red 'a 'b (op: b -> b -> b) (f: []a -> b) (as: []a): b =
  intrinsics.stream_red (op, f, as)

let stream_red_per 'a 'b (op: b -> b -> b) (f: []a -> b) (as: []a): b =
  intrinsics.stream_red_per (op, f, as)

let stream_map 'a 'b (f: []a -> []b) (as: []a): *[]b =
  intrinsics.stream_map (f, as)

let stream_map_per 'a 'b (f: []a -> []b) (as: []a): *[]b =
  intrinsics.stream_map_per (f, as)

-- | Combines the outer two dimensions of an array.
let flatten [n][m] 't (xs: [n][m]t): []t =
  reshape (n*m) xs

-- | Combines the outer three dimensions of an array.
let flatten_3d [n][m][l] 't (xs: [n][m][l]t): []t =
  reshape (n*m*l) xs

-- | Splits the outer dimension of an array in two.
let unflatten 't (n: i32) (m: i32) (xs: []t): [n][m]t =
  reshape (n,m) xs

-- | Splits the outer dimension of an array in three.
let unflatten_3d 't (n: i32) (m: i32) (l: i32) (xs: []t): [n][m][l]t =
  reshape (n,m,l) xs

let intersperse [n] 't (x: t) (xs: [n]t): *[]t =
  map (\i -> if i % 2 == 1 && i != 2*n then x
             else unsafe xs[i/2])
      (iota (i32.max (2*n-1) 0))

let intercalate [n] [m] 't (x: [m]t) (xs: [n][m]t): []t =
  unsafe flatten (intersperse x xs)

let transpose [n] [m] 't (a: [n][m]t): [m][n]t =
  rearrange (1,0) a

let steps (start: i32) (num_steps: i32) (step: i32): [num_steps]i32 =
  map (start+) (map (step*) (iota num_steps))

let range (start: i32) (end: i32) (step: i32): []i32 =
  let w = (end-start)/step
  in steps start w step

-- | True if all of the input elements are true.
let and (xs: []bool): bool = reduce (&&) true xs

-- | True if any of the input elements are true.
let or (xs: []bool): bool = reduce (||) false xs

let pick [n] 't (flags: [n]bool) (xs: [n]t) (ys: [n]t): *[n]t =
  map3 (\flag x y -> if flag then x else y) flags xs ys
