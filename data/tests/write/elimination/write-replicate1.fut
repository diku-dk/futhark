-- Test that a multiple replicates can be eliminated in a write.
-- ==
-- input {
--   [0, 3, 1]
--   [9, 8, -3, 90, 41]
--   [9, 8, -3, 90, 41]
-- }
-- output {
--   [5, 5, -3, 5, 41]
--   [10, 10, -3, 10, 41]
-- }
-- structure { Write 1 }

fun main(indexes: [k]i32,
       array0: *[n]i32,
       array1: *[n]i32): ([n]i32, [n]i32) =
  write (indexes, indexes)
        (replicate k 5, replicate k 10)
        (array0, array1)
