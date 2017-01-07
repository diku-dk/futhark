-- Make sure inner shape names are available, even if they are
-- "shadowed" by an outer type ascription.
--
-- ==
-- input { 2 [1,2,3] }
-- output { 4 }

fun main ((_, elems: [n]int): (int,[]int)): int =
  n + elems[0]
