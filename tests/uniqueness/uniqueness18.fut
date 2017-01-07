-- When the map is simplified away, it must turn into a copy, as the
-- result is consumed.
--
-- ==
-- structure { Map 0 Copy 1 }

fun main(as: []int): *[]int =
  map (fn x => x) as
