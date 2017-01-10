-- See if we can access an array with a stride.
--
-- ==
-- input { [0,1,2,3,4,5,6,7,8,9] 4 9 2 } output { [4,6,8] }
-- input { [0,1,2,3,4,5,6,7,8,9] 9 4 -2 } output { [9,7,5] }
-- input { [0,1,2,3,4,5,6,7,8,9] 9 -10 -2 } error: Assertion.*failed

fun main(as: []int, i: int, j: int, s: int): []int =
  as[i:j:s]
