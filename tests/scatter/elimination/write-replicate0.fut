-- Test that a replicate can be eliminated in a write.
-- ==
-- input {
--   [0i64, 3i64, 1i64]
--   [9, 8, -3, 90, 41]
-- }
-- output {
--   [5, 5, -3, 5, 41]
-- }
-- structure { Screma 1 }

def main [k] [n] (indexes: [k]i64) (array: *[n]i32) : [n]i32 =
  scatter array indexes (replicate k 5)
