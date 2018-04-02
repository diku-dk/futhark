-- | Utility functions for arrays.

import "/futlib/math"
import "/futlib/soacs"
import "/futlib/functional"

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

-- | Concatenate two arrays.  Warning: never try to perform a reduction
-- with this operator; it will not work.
let (++) 't (xs: []t) (ys: []t): *[]t = concat xs ys

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

-- | Combines the outer two dimensions of an array.
let flatten [n][m] 't (xs: [n][m]t): []t =
  reshape (n*m) xs

-- | Combines the outer three dimensions of an array.
let flatten_3d [n][m][l] 't (xs: [n][m][l]t): []t =
  reshape (n*m*l) xs

-- | Combines the outer four dimensions of an array.
let flatten_4d [n][m][l][k] 't (xs: [n][m][l][k]t): []t =
  reshape (n*m*l*k) xs

-- | Splits the outer dimension of an array in two.
let unflatten 't (n: i32) (m: i32) (xs: []t): [n][m]t =
  reshape (n,m) xs

-- | Splits the outer dimension of an array in three.
let unflatten_3d 't (n: i32) (m: i32) (l: i32) (xs: []t): [n][m][l]t =
  reshape (n,m,l) xs

-- | Splits the outer dimension of an array in four.
let unflatten_4d 't (n: i32) (m: i32) (l: i32) (k: i32) (xs: []t): [n][m][l][k]t =
  reshape (n,m,l,k) xs

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
let and: []bool -> bool = all id

-- | True if any of the input elements are true.
let or: []bool -> bool = any id

let pick [n] 't (flags: [n]bool) (xs: [n]t) (ys: [n]t): *[n]t =
  map3 (\flag x y -> if flag then x else y) flags xs ys

-- | Perform a *sequential* left-fold of an array.
let foldl 'a 'b (f: a -> b -> a) (acc: a) (bs: []b): a =
  loop acc for b in bs do f acc b

-- | Perform a *sequential* right-fold of an array.
let foldr 'a 'b (f: b -> a -> a) (acc: a) (bs: []b): a =
  foldl (flip f) acc (reverse bs)

-- | Create a value for each point in a one-dimensional index space.
let tabulate 'a (n: i32) (f: i32 -> a): *[n]a =
  map1 f (iota n)

-- | Create a value for each point in a two-dimensional index space.
let tabulate_2d 'a (n: i32) (m: i32) (f: i32 -> i32 -> a): *[n][m]a =
  map1 (tabulate m <<| f) (iota n)
