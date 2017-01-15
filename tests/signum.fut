-- Test the sign function prefix operator.
--
-- ==
-- input { 2 0 -3 [1,-2,3,0] }
-- output { 1 0 -1 [1,-1,1,0] }

fun main(x: i32, y: i32, z: i32, a: []i32): (i32,i32,i32,[]i32) =
  (signum x,
   signum y,
   signum z,
   map signum a)
