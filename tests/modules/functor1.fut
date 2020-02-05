-- A functor with a signature containing an abstract type.
-- ==
-- input { [1,2,3] [4,5,6] [1f32,2f32,3f32] [4f32,5f32,6f32] }
-- output { 315 315f32 }

module type NUMERIC = {
  type num
  val plus : num -> num -> num
  val mult : num -> num -> num
  val one : num
  val zero : num
}

module Int = {
  type num = i32
  let plus (x: i32) (y: i32): i32 = x + y
  let mult (x: i32) (y: i32): i32 = x * y
  let one: i32 = 1
  let zero: i32 = 0
}

module Float32 = {
  type num = f32
  let plus (x: f32) (y: f32): f32 = x + y
  let mult (x: f32) (y: f32): f32 = x * y
  let one: f32 = 1f32
  let zero: f32 = 0f32
}

module DotProd(T: NUMERIC) = {
  let dotprod [n] (xs: [n]T.num) (ys: [n]T.num): T.num =
    reduce T.mult T.one (map2 T.plus xs ys)
}

module IntDotProd = DotProd(Int)
module Float32DotProd = DotProd(Float32)

let main [n]
        (xs: [n]i32) (ys: [n]i32)
        (as: [n]f32) (bs: [n]f32): (i32, f32) =
  (IntDotProd.dotprod xs ys,
   Float32DotProd.dotprod as bs)
