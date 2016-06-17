-- Test that write-write fusion is *not* applied when one write uses the output
-- of another write.
-- ==
-- structure { Write 2 }

fun [n]i32
  main([k]i32 indexes,
       [k]i32 values1,
       [k]i32 values2,
       *[n]i32 array) =
  let array' = write(indexes, values1, array)
  let array'' = write(indexes, values2, array')
  in array''
