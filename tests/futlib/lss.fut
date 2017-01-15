-- ==
-- input { [1, -2, -2, 3, 4, -1, 5, -6, 1] }
-- output { 4 3 }
-- input { [5, 4, 3, 2, 1] }
-- output { 1 5 }
-- input { [1, 2, 3, 4, 5] }
-- output { 5 1 }

include futlib.lss

module Ascending = {
  type t = i32

  val blank = 0
  fun pred1 (x: i32) = true
  fun pred2 (x: i32) (y: i32) = x <= y
}

module Descending = {
  type t = i32

  val blank = 0
  fun pred1 (x: i32) = true
  fun pred2 (x: i32) (y: i32) = x >= y
}

module LSS_ascending = LSS(Ascending)
module LSS_descending = LSS(Descending)

fun main(xs: []i32): (i32,i32) =
  (LSS_ascending.lss xs,
   LSS_descending.lss xs)
