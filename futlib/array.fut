-- Utility functions for arrays.

import "/futlib/math"

-- | The size of the outer dimensionion of an array.
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
