-- Test that map-scatter fusion works in a slightly less simple case.
-- ==
-- input {
--   [2, 0]
--   [100, 200]
--   [0, 2, 4, 6, 9]
-- }
-- output {
--   [200, 2, 102, 6, 9]
-- }
-- structure { Scatter 1 }

let main [k][n] (indexes: [k]i32,
                 values: [k]i32,
                 array: *[n]i32): [n]i32 =
  let values' = map (+) indexes values
  let array' = scatter array indexes values'
  in array'
