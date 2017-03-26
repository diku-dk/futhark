-- ==
-- input { [1, -2, 3, 4, -1, 5, -6, 1] }
-- output { 11 10 }

import "futlib/math"
import "futlib/mss"

module Int_measure = {
  type t = i32
  type m = i32

  let zero = 0
  let max (x: i32) (y: i32) = i32.max x y
  let combine (x: i32) (y: i32) = x + y
  let single (x: i32) = x
}

module Weird_measure = {
  type t = i32
  type m = (i32, i32)

  let asInt ((x,xlen): m) = x + xlen

  let zero = (0, 0)
  -- Is this max actually associative?
  let max (x: m) (y: m) = if asInt x < asInt y then y else x
  let combine ((x,xlen): m) ((y,ylen): m) = (x+y, xlen+ylen)
  let single (x: i32) = (x, 1)
}

module MSS_Int = MSS(Int_measure)
module MSS_Weird = MSS(Weird_measure)

let main(xs: []i32): (i32, i32) =
  (MSS_Int.mss xs,
   #1 (MSS_Weird.mss xs))
