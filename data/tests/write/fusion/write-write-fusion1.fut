-- Test that write-write fusion works with more than two arrays.
-- ==
-- input {
--   [0]
--   [99]
--   [10, 20, 30, 40, 50]
--   [100, 200, 300, 400, 500]
--   [1000, 2000, 3000, 4000, 5000]
-- }
-- output {
--   [99, 20, 30, 40, 50]
--   [99, 200, 300, 400, 500]
--   [99, 2000, 3000, 4000, 5000]
-- }
-- structure { Write 1 }

fun ([i32, n], [i32, n], [i32, n])
  main([i32, k] indexes,
       [i32, k] values,
       *[i32, n] array1,
       *[i32, n] array2,
       *[i32, n] array3) =
  let array1' = write(indexes, values, array1)
  let array2' = write(indexes, values, array2)
  let array3' = write(indexes, values, array3)
  in (array1', array2', array3')
