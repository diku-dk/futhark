-- Split on an innermost dimension.
-- ==
-- input { [[1,2], [3,4], [5,6]] }
-- output { [[1], [3], [5]]
--          [[2], [4], [6]]
-- }

fun main(xs: [][]int): ([][]int, [][]int) =
  split@1((1), xs)
