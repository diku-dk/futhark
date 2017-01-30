-- Test that map-write fusion works in a simple case.
-- ==
-- input {
--   [2, 0]
--   [100, 200]
--   [0, 2, 4, 6, 9]
-- }
-- output {
--   [0, 200, 4, 100, 9]
-- }
-- structure { Write 1 }

fun main(indexes: [k]i32,
       values: [k]i32,
       array: *[n]i32): [n]i32 =
  let indexes' = map (+1) indexes
  let array' = write indexes' values array
  in array'
