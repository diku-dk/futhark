-- Concatenating across innermost dimension.
--
-- ==
-- input { [[1,2],[4,5],[7,8]] [[3],[6],[9]] }
-- output { [[1,2,3],[4,5,6],[7,8,9]] }
--
-- input { [[1,2],[4,5],[7,8]] [[3,2,1],[6,5,4],[9,8,7]] }
-- output { [[1,2,3,2,1],[4,5,6,5,4],[7,8,9,8,7]] }

fun main(xs: [n][]int, ys: [n][]int): [n][]int =
  concat@1(xs, ys)
