-- ==
-- input { [1, -2, 3, 4, -1, 5, -6, 1] }
-- output { 11 10 }

include futlib.mss

module Int_measure = {
  type t = int
  type m = int

  val zero = 0
  fun max (x: int) (y: int) = if x < y then y else x
  fun combine (x: int) (y: int) = x + y
  fun single (x: int) = x
}

module Weird_measure = {
  type t = int
  type m = (int, int)

  fun asInt ((x,xlen): m) = x + xlen

  val zero = (0, 0)
  -- Is this max actually associative?
  fun max (x: m) (y: m) = if asInt x < asInt y then y else x
  fun combine ((x,xlen): m) ((y,ylen): m) = (x+y, xlen+ylen)
  fun single (x: int) = (x, 1)
}

module MSS_Int = MSS(Int_measure)
module MSS_Weird = MSS(Weird_measure)

fun main(xs: []int): (int, int) =
  (MSS_Int.mss xs,
   #0 (MSS_Weird.mss xs))
