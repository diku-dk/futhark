-- Test the sign function prefix operator.
--
-- ==
-- input { 2 0 -3 [1,-2,3,0] }
-- output { 1 0 -1 [1,-1,1,0] }

fun main(x: int, y: int, z: int, a: []int): (int,int,int,[]int) =
  (signum x,
   signum y,
   signum z,
   map(signum,a))
