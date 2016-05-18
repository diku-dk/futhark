-- Test that write-write fusion works in a simple case.
-- ==
-- input {
--   [1, 0]
--   [8, 2]
--   [5, 3]
--   [10, 20, 30, 40, 50]
--   [100, 200, 300, 400, 500]
-- }
-- output {
--   [2, 8, 30, 40, 50]
--   [3, 5, 300, 400, 500]
-- }
-- structure { Write 1 }

fun ([i32, n], [i32, n])
  main([i32, k] indexes,
       [i32, k] values1,
       [i32, k] values2,
       *[i32, n] array1,
       *[i32, n] array2) =
  let array1' = write(indexes, values1, array1)
  let array2' = write(indexes, values2, array2)
  in (array1', array2')
