-- A shape declaration referring to a non-integer value should be an
-- error.
--
-- ==
-- error: declaration.*integer

fun main(as: []int, b: bool): [][]int =
  map (\i: [b]int -> replicate 3 i) as
