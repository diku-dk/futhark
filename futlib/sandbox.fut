module type EQ = {
type t
val eq: t -> t -> bool
}

module type ORD = {
type t
val eq: t -> t -> bool
val lt: t -> t -> bool
val gt: t -> t -> bool
}

module type NUMERIC = {
type t
val plus: t -> t -> t
val sub: t -> t -> t
val mult: t -> t -> t
val one: t
val zero: t
val eq: t -> t -> bool
val lt: t -> t -> bool
val gt: t -> t -> bool
}

module I32 = {
type t = i32

fun plus (x: i32) (y: i32) = x + y
fun sub  (x: i32) (y: i32) = x - y
fun mult (x: i32) (y: i32) = x * y

val one = 1
val zero = 0

fun eq (x: i32) (y: i32) = x == y
fun lt (x: i32) (y: i32) = x < y
fun gt (x: i32) (y: i32) = x > y
}

module F32 = {
type t = f32

fun plus (x: f32) (y: f32) = x + y
fun sub  (x: f32) (y: f32) = x - y
fun mult (x: f32) (y: f32) = x * y

val one = 1f32
val zero = 0f32

fun eq (x: f32) (y: f32) = x == y
fun lt (x: f32) (y: f32) = x < y
fun gt (x: f32) (y: f32) = x > y
}

module Ordering(T: ORD) = {
fun max (x: T.t) (y: T.t) =
  if T.gt x y then x else y

fun min (x: T.t) (y: T.t) =
  if T.gt x y then y else x

fun max_elem (x: T.t) (xs: [n]T.t) =
  reduce max x xs

fun min_elem (x: T.t) (xs: [n]T.t) =
  reduce min x xs
}

module Numerics(T: NUMERIC) = {
fun fact (n: T.t): T.t =
  if T.eq n T.zero
  then T.one
  else T.mult n (fact (T.sub n T.one))

fun dotprod (xs: [n]T.t) (ys: [n]T.t): T.t =
  reduce T.mult T.one (map T.plus xs ys)

fun matmult (xss: [n][p]T.t) (yss: [p][m]T.t): [n][m]T.t =
  map (\xs -> map (dotprod xs) (transpose yss)) xss
}
