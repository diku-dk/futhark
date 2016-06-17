-- Expected distributed structure:
--
-- map
--   map
-- map
--   map
--
-- ==
-- structure distributed { MapKernel 2 Map 0 }

fun []([]int,[]int) main([n][an]int a, [n][bn]int b) =
  zipWith(fn ([an]int,[bn]int) ([]int a_row, []int b_row) =>
            (map(+1, a_row),
             map(-1, b_row)),
          a, b)
