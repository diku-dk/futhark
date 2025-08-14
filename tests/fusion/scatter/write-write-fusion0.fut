-- Test that write-write fusion works in a simple case.
-- ==
-- input {
--   [1i64, 0i64]
--   [8, 2]
--   [5, 3]
--   [10, 20, 30, 40, 50]
--   [100, 200, 300, 400, 500]
-- }
-- output {
--   [2, 8, 30, 40, 50]
--   [3, 5, 300, 400, 500]
-- }
-- structure { Screma 1 }

def main [n] [k]
         (indexes: [k]i64)
         (values1: [k]i32)
         (values2: [k]i32)
         (array1: *[n]i32)
         (array2: *[n]i32) : ([n]i32, [n]i32) =
  let array1' = scatter array1 indexes values1
  let array2' = scatter array2 indexes values2
  in (array1', array2')
