-- Test that write works with tuples.
-- ==
--
-- input {
--   [1]
--   [3]
--   [7]
--   [1, 10, 99]
--   [2, 20]
-- }
-- output {
--   [1, 3, 99]
--   [2, 7]
-- }

fun main(indexes: [k]i32,
       valuesA: [k]i32,
       valuesB: [k]i32,
       arrayA: *[m]i32,
       arrayB: *[n]i32): ([m]i32, [n]i32) =
  write (zip indexes indexes) (zip valuesA valuesB) (arrayA, arrayB)
