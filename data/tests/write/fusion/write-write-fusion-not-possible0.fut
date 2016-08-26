-- Test that write-write fusion is *not* applied when one write uses the output
-- of another write.
-- ==
-- structure { Write 2 }

fun main(indexes: [k]i32,
       values1: [k]i32,
       values2: [k]i32,
       array: *[n]i32): [n]i32 =
  let array' = write(indexes, values1, array)
  let array'' = write(indexes, values2, array')
  in array''
