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

module i32 = {
type t = i32

let plus (x: i32) (y: i32) = x + y
let sub  (x: i32) (y: i32) = x - y
let mult (x: i32) (y: i32) = x * y

val one = 1
val zero = 0

let eq (x: i32) (y: i32) = x == y
let lt (x: i32) (y: i32) = x < y
let gt (x: i32) (y: i32) = x > y
}

module f32 = {
type t = f32

let plus (x: f32) (y: f32) = x + y
let sub  (x: f32) (y: f32) = x - y
let mult (x: f32) (y: f32) = x * y

val one = 1f32
val zero = 0f32

let eq (x: f32) (y: f32) = x == y
let lt (x: f32) (y: f32) = x < y
let gt (x: f32) (y: f32) = x > y
}

module Ordering(T: ORD) = {
let max (x: T.t) (y: T.t) =
  if T.gt x y then x else y

let min (x: T.t) (y: T.t) =
  if T.gt x y then y else x

let max_elem (x: T.t) (xs: [n]T.t) =
  reduce max x xs

let min_elem (x: T.t) (xs: [n]T.t) =
  reduce min x xs
}

module Numerics(T: NUMERIC) = {
let fact (n: T.t): T.t =
  if T.eq n T.zero
  then T.one
  else T.mult n (fact (T.sub n T.one))

let dotprod (xs: [n]T.t) (ys: [n]T.t): T.t =
  reduce T.mult T.one (map T.plus xs ys)

let matmult (xss: [n][p]T.t) (yss: [p][m]T.t): [n][m]T.t =
  map (\xs -> map (dotprod xs) (transpose yss)) xss
}
