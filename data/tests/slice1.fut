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

fun [n][]int main([n][m]int as, int i, int j) =
  as[0:n,i:j]
