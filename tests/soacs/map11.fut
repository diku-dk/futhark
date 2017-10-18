-- Test a simple map, that might be irregular based on input data.
-- ==
-- tags { no_opencl }
-- input {
--   [8,8,8,8]
-- }
-- output {
--   [[0, 1, 2, 3, 4, 5, 6, 7], [0, 1, 2, 3, 4, 5, 6, 7], [0, 1, 2, 3, 4, 5, 6, 7], [0, 1, 2, 3, 4, 5, 6, 7]]
-- }
let main(a: []i32): [][]i32 =
  map (\(n: i32): []i32  -> iota(n)) a
