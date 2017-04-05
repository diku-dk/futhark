-- Test that write-write fusion is *not* applied when one write uses the output
-- of another write.
-- ==
-- structure { Scatter 2 }

let main(indexes: [#k]i32,
         values1: [#k]i32,
         values2: [#k]i32,
         array: *[#n]i32): [n]i32 =
  let array' = scatter array indexes values1
  let array'' = scatter array' indexes values2
  in array''
