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

let scatter 't [m] [n] (dest: *[m]t) (is: [n]i32) (vs: [n]t): *[m]t =
  intrinsics.scatter dest is vs

let pick [n] 't (flags: [n]bool) (xs: [n]t) (ys: [n]t): *[n]t =
  map (\flag x y -> if flag then x else y) flags xs ys

let flatten [n] [m] 't (xs: [n][m]t): []t =
  reshape (n*m) xs

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

-- | Apply the given function to each element of a single array.
let map1 'a [n] 'x (f: a -> x) (as: [n]a): [n]x =
  map f as

-- | As 'map1', but with one more array.
let map2 'a 'b [n] 'x (f: a -> b -> x) (as: [n]a) (bs: [n]a): [n]x =
  map (\(a, b) -> f a b) (zip as bs)

-- | As 'map2', but with one more array.
let map3 'a 'b 'c [n] 'x (f: a -> b -> c -> x) (as: [n]a) (bs: [n]a) (cs: [n]a): [n]x =
  map (\(a, b, c) -> f a b c) (zip as bs cs)

-- | As 'map3', but with one more array.
let map4 'a 'b 'c 'd [n] 'x (f: a -> b -> c -> d -> x) (as: [n]a) (bs: [n]a) (cs: [n]a) (ds: [n]a): [n]x =
  map (\(a, b, c, d) -> f a b c d) (zip as bs cs ds)

-- | As 'map4', but with one more array.
let map5 'a 'b 'c 'd 'e [n] 'x (f: a -> b -> c -> d -> e -> x) (as: [n]a) (bs: [n]a) (cs: [n]a) (ds: [n]a) (es: [n]a): [n]x =
  map (\(a, b, c, d, e) -> f a b c d e) (zip as bs cs ds es)
