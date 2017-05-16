-- Utility functions for arrays.

import "/futlib/math"

let length [n] 't (_: [n]t) = n

let null [n] 't (_: [n]t) = n == 0

let head [n] 't (x: [n]t) = x[0]

let tail [n] 't (x: [n]t) = x[1:]

let init [n] 't (x: [n]t) = x[0:n-1]

let last [n] 't (x: [n]t) = x[n-1]

let take [n] 't (i: i32) (x: [n]t): [i]t = x[0:i]

let drop [n] 't (i: i32) (x: [n]t) = x[i:]

let reverse [n] 't (x: [n]t): [n]t = x[::-1]

let update [n] 't (xs: *[n]t) (i: i32) (x: t): [n]t = xs with [i] <- x

let pick [n] 't (flags: [n]bool) (xs: [n]t) (ys: [n]t): [n]t =
  map (\flag x y -> if flag then x else y) flags xs ys

let flatten [n] [m] 't (xs: [n][m]t): []t =
  reshape (n*m) xs

let intersperse [n] 't (x: t) (xs: [n]t): []t =
  unsafe
  map (\i -> if i % 2 == 1 && i != 2*n then x
             else xs[i/2])
      (iota (i32.max (2*n-1) 0))

let intercalate [n] [m] 't (x: [m]t) (xs: [n][m]t): []t =
  flatten (intersperse x xs)

let transpose [n] [m] 't (a: [n][m]t): [m][n]t =
  rearrange (1,0) a

let copy [n] 't (a: [n]t): *[n]t =
  map (\x -> x) a
