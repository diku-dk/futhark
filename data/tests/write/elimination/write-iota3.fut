-- Test that multiple iotas can be eliminated in a write with multiple arrays.
-- ==
-- input {
--   3
--   [100, 200, 300, 400, 500, 600, 700]
--   [100, 200, 300, 400, 500, 600, 700]
-- }
-- output {
--   [3, 4, 5, 400, 500, 600, 700]
--   [3, 4, 300, 400, 500, 600, 700]
-- }
-- structure { Write 1 }

fun main(k: i32, array0: *[n]i32, array1: *[n]i32): ([n]i32, [n]i32) =
  write (iota(k), map (-1) (iota(k)))
        (map (+3) (iota(k)), map (+2) (iota(k)))
        (array0, array1)
