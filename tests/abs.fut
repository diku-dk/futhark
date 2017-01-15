-- Test the absolute value prefix operator.
--
-- ==
-- input { 2 -3 [1,-2,3] }
-- output { 2 3 [1,2,3] }

fun main(x: i32, y: i32, a: []i32): (i32,i32,[]i32) =
  (abs x,
   abs y,
   map abs a)
