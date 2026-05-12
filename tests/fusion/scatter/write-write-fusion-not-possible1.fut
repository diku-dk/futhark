-- Test that write-write fusion is *not* applied when one write uses the output
-- of another write.
-- ==
-- structure { Screma 2 }

def main [k]
         ( indexes: [k]i64
         , values1: [k]i64
         , values2: [k]i64
         , array1: *[k]i64
         , array2: *[k]i64
         ) : [k]i64 =
  let array1' = scatter array1 indexes values1
  let array2' = scatter array2 array1' values2
  in array2'
