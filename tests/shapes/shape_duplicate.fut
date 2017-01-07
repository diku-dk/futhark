-- Make sure inner shape names are available, even if they are
-- "shadowed" by an outer named shape.
--
-- ==
-- input { 2 [1,2,3] }
-- output { 7 }

fun main ((_, elems: [n]int): (int,[m]int)): int =
  n + m + elems[0]
