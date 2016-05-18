-- Test that write-write fusion is *not* applied when one write uses the output
-- of another write.
-- ==
-- structure { Write 2 }

fun [i32, n]
  main([i32, k] indexes,
       [i32, k] values1,
       [i32, k] values2,
       *[i32, n] array) =
  let array' = write(indexes, values1, array)
  let array'' = write(indexes, values2, array')
  in array''
