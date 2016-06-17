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

fun ([m]i32, [n]i32)
  main([k]i32 indexes,
       [k]i32 valuesA,
       [k]i32 valuesB,
       *[m]i32 arrayA,
       *[n]i32 arrayB) =
  write(zip(indexes, indexes), zip(valuesA, valuesB), arrayA, arrayB)
