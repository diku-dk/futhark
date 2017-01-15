-- ==
-- input { [1, -2, 3, 4, -1, 5, -6, 1] }
-- output { 11 10 }

include futlib.mss

module Int_measure = {
  type t = i32
  type m = i32

  val zero = 0
  fun max (x: i32) (y: i32) = if x < y then y else x
  fun combine (x: i32) (y: i32) = x + y
  fun single (x: i32) = x
}

module Weird_measure = {
  type t = i32
  type m = (i32, i32)

  fun asInt ((x,xlen): m) = x + xlen

  val zero = (0, 0)
  -- Is this max actually associative?
  fun max (x: m) (y: m) = if asInt x < asInt y then y else x
  fun combine ((x,xlen): m) ((y,ylen): m) = (x+y, xlen+ylen)
  fun single (x: i32) = (x, 1)
}

module MSS_Int = MSS(Int_measure)
module MSS_Weird = MSS(Weird_measure)

fun main(xs: []i32): (i32, i32) =
  (MSS_Int.mss xs,
   #0 (MSS_Weird.mss xs))
