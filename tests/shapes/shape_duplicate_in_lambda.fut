-- Make sure inner shape names are available, even if they are
-- "shadowed" by an outer named shape, even in a lambda.
--
-- ==
-- input { [[1,2],[3,4]] }
-- output { [4, 4] }

fun main (xss: [][]int): []int =
  map (\((_xs: [m]int): [n]int): int -> n + m) xss
