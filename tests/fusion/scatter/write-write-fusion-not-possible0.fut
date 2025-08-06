-- Test that write-write fusion is *not* applied when one write uses the output
-- of another write.
-- ==
-- input { [-1i64, 0i64] [0i64,1i64] [1,1] [2,2] [0] }
-- output { [2] }
-- structure { Screma 1 }

def main [k] [n]
         (indexes1: [k]i64)
         (indexes2: [k]i64)
         (values1: [k]i32)
         (values2: [k]i32)
         (array: *[n]i32) : [n]i32 =
  let array' = scatter array indexes1 values1
  let array'' = scatter array' indexes2 values2
  in array''
