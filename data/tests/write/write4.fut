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

fun ([i32, m], [i32, n])
  main([i32, k] indexes,
       [i32, k] valuesA,
       [i32, k] valuesB,
       *[i32, m] arrayA,
       *[i32, n] arrayB) =
  write(zip(indexes, indexes), zip(valuesA, valuesB), arrayA, arrayB)
