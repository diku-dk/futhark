-- Distribute a redomap inside of a map.
--
-- One possible structure:
--
-- map
--   map
-- map
--   reduce
--
-- Currently expected structure:
--
-- map
--   loop
-- ==
--
-- structure distributed { Kernel 1 }

fun main(a: [][]int): []int =
  map(fn (a_r: []int): int  =>
        reduce(+, 0, map(+1, a_r)),
      a)
