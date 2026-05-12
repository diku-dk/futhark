-- Test that map-scatter fusion and scatter-scatter fusion work together.
-- ==
-- input {
--   [2i64, 0i64]
--   [1i64, 0i64]
--   [100, 80]
--   [0, 2, 4, 6, 9]
--   [10, 12, 14, 16, 19]
-- }
-- output {
--   [84i32, 2i32, 104i32, 6i32, 9i32]
--   [240i32, 300i32, 14i32, 16i32, 19i32]
-- }
-- structure { Screma 1 }

def main [k] [n]
         (indexes0: [k]i64)
         (indexes1: [k]i64)
         (values: [k]i32)
         (array0: *[n]i32)
         (array1: *[n]i32) : ([n]i32, [n]i32) =
  let values0' = map (+ 4) values
  let values1' = map (* 3) values
  let array0' = scatter array0 indexes0 values0'
  let array1' = scatter array1 indexes1 values1'
  in (array0', array1')
