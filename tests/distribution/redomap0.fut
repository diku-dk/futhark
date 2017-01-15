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

fun main(a: [][]i32): []i32 =
  map (\(a_r: []i32): i32  ->
        reduce (+) 0 (map (+1) (a_r))) a
