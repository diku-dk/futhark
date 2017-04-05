-- Test that write-write fusion is *not* applied when one write uses the output
-- of another write.
-- ==
-- structure { Scatter 2 }

let main(indexes: [#k]i32,
       values1: [#k]i32,
       values2: [#k]i32,
       array1: *[#k]i32,
       array2: *[#k]i32): [k]i32 =
  let array1' = scatter array1 indexes values1
  let array2' = scatter array2 array1' values2
  in array2'
