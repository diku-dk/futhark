-- Test that map-write fusion works in a simple case.
-- ==
-- input {
--   [2i64, 0i64]
--   [100i64, 200i64]
--   [0i64, 2i64, 4i64, 6i64, 9i64]
-- }
-- output {
--   [0i64, 200i64, 4i64, 100i64, 9i64]
-- }
-- structure { Screma 1 }

def main [k] [n]
         (indexes: [k]i64)
         (values: [k]i64)
         (array: *[n]i64) : [n]i64 =
  let indexes' = map (+ 1) indexes
  let array' = scatter array indexes' values
  in array'
