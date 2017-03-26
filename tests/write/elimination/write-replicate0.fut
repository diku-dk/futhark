-- Test that a replicate can be eliminated in a write.
-- ==
-- input {
--   [0, 3, 1]
--   [9, 8, -3, 90, 41]
-- }
-- output {
--   [5, 5, -3, 5, 41]
-- }
-- structure { Write 1 }

let main(indexes: [k]i32,
       array: *[n]i32): [n]i32 =
  write indexes (replicate k 5) (array)
