-- Test that write-write fusion is *not* applied when one write uses the output
-- of another write.
-- ==
-- structure { Write 2 }

fun [i32, k]
  main([i32, k] indexes,
       [i32, k] values1,
       [i32, k] values2,
       *[i32, k] array1,
       *[i32, k] array2) =
  let array1' = write(indexes, values1, array1)
  let array2' = write(array1', values2, array2)
  in array2'
