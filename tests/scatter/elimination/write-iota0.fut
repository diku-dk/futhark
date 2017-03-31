-- Test that an iota can be eliminated in a write.  Contrived example.
-- ==
-- input {
--   [100, 200, 300]
--   [5, 10, 15, 20, 25, 30]
-- }
-- output {
--   [100, 200, 300, 20, 25, 30]
-- }
-- structure { Scatter 1 }

let main(values: [k]i32,
       array: *[n]i32): [n]i32 =
  scatter array (iota k) values
