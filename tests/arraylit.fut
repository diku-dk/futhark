-- Array literals should work even if their safety cannot be
-- determined until runtime.
--
-- ==
-- input { 2 2 } output { [[0,1], [3, 3]] }
-- input { 2 3 } error: failed

fun main (n: int, m: int): [][]int =
  [iota n, replicate m 3]
