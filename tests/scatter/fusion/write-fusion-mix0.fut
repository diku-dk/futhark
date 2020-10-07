-- Test that map-scatter fusion and scatter-scatter fusion work together.
-- ==
-- input {
--   [2i64, 0i64]
--   [1i64, 0i64]
--   [100, 80]
--   [90, 80]
--   [0, 2, 4, 6, 9]
--   [10, 12, 14, 16, 19]
-- }
-- output {
--   [84, 2, 104, 6, 9]
--   [240, 270, 14, 16, 19]
-- }
-- structure { Scatter 1 }

let main [k][n] (indexes0: [k]i64)
                (indexes1: [k]i64)
                (values0: [k]i32)
                (values1: [k]i32)
                (array0: *[n]i32)
                (array1: *[n]i32): ([n]i32, [n]i32) =
  let values0' = map (+4) values0
  let values1' = map (*3) values1
  let array0' = scatter array0 indexes0 values0'
  let array1' = scatter array1 indexes1 values1'
  in (array0', array1')
