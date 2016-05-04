-- Test that write works with tuples.
-- ==
--
-- input {
--   [1]
--   [3]
--   [7]
--   [1, 10]
--   [2, 20]
-- }
-- output {
--   [1, 3]
--   [2, 7]
-- }

fun ([i32, n], [i32, n])
  main([i32, k] indexes,
       [i32, k] valuesA,
       [i32, k] valuesB,
       *[i32, n] arrayA,
       *[i32, n] arrayB) =
  unzip(run(indexes, zip(valuesA, valuesB), copy(zip(arrayA, arrayB))))

fun [(i32, i32), n]
  run([i32, k] indexes,
       [(i32, i32), k] values,
       *[(i32, i32), n] array) =
  write(indexes, values, array)
