-- Single-iteration maps should be simplified away.
--
-- ==
-- input { 2 } output { [4] }
-- structure { Map 0 }

fun main(x: int): [1]int =
  map((+x), replicate(1, x))
