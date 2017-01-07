-- Test the absolute value prefix operator.
--
-- ==
-- input { 2 -3 [1,-2,3] }
-- output { 2 3 [1,2,3] }

fun main(x: int, y: int, a: []int): (int,int,[]int) =
  (abs x,
   abs y,
   map abs a)
