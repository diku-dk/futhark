-- Test that write-write fusion is *not* applied when one write uses the output
-- of another write.
-- ==
-- structure { Write 2 }

fun main(indexes: [k]i32,
       values1: [k]i32,
       values2: [k]i32,
       array1: *[k]i32,
       array2: *[k]i32): [k]i32 =
  let array1' = write indexes values1 (array1)
  let array2' = write array1' values2 (array2)
  in array2'
