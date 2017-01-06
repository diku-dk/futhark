module type NUMERIC {
type num
val plus: num -> num -> num
val sub: num -> num -> num
val mult: num -> num -> num
val one: num
val zero: num
val eq: num -> num -> bool
val lt: num -> num -> bool
val gt: num -> num -> bool
}

module I32 {
type num = int
fun plus (x: int) (y: int): int = x + y
fun sub (x: int) (y: int): int = x - y
fun mult (x: int) (y: int): int = x * y
val one: int = 1
val zero: int = 0
fun eq (x: int) (y: int): bool = x == y
fun lt (x: int) (y: int): bool = x < y
fun gt (x: int) (y: int): bool = x > y
}

module F32 {
type num = f32
fun plus (x: f32) (y: f32): f32 = x + y
fun sub (x: f32) (y: f32): f32 = x - y
fun mult (x: f32) (y: f32): f32 = x * y
val one: f32 = 1f32
val zero: f32 = 0f32
fun eq (x: f32) (y: f32): bool = x == y
fun lt (x: f32) (y: f32): bool = x < y
fun gt (x: f32) (y: f32): bool = x > y
}

module Numerics(T: NUMERIC) {
fun fact (n: T.num): T.num =
  if T.eq n T.zero
  then T.one
  else T.mult n (fact (T.sub n T.one))

fun dotprod (xs: [n]T.num) (ys: [n]T.num): T.num =
  reduce T.mult T.one (map T.plus xs ys)
}
