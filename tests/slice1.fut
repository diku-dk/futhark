-- Slicing a multidimensional array across the outer dimension.
--
-- ==
-- input { [[1,2,3],[4,5,6]] 1 3 }
-- output { [[2,3],[5,6]] }
-- input { [[1,2,3],[4,5,6]] 0 3 }
-- output { [[1,2,3],[4,5,6]] }
-- input { [[1,2,3],[4,5,6]] 1 1 }
-- output { empty([]int) }
-- input { [[1,2,3],[4,5,6]] 1 0 }
-- error: Assertion.*failed

fun main(as: [n][m]int, i: int, j: int): [n][]int =
  as[0:n,i:j]
