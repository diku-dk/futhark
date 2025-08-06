-- Test that scatter-scatter fusion works with more than two arrays.
-- ==
-- input {
--   [0i64]
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
-- structure { Screma 1 }

def main [k] [n]
         (indexes: [k]i64)
         (values: [k]i32)
         (array1: *[n]i32)
         (array2: *[n]i32)
         (array3: *[n]i32) : ([n]i32, [n]i32, [n]i32) =
  let array1' = scatter array1 indexes values
  let array2' = scatter array2 indexes values
  let array3' = scatter array3 indexes values
  in (array1', array2', array3')
