-- Test that map-scatter fusion works in a slightly less simple case.
-- ==
-- input {
--   [2i64, 0i64]
--   [100i64, 200i64]
--   [0i64, 2i64, 4i64, 6i64, 9i64]
-- }
-- output {
--   [200i64, 2i64, 102i64, 6i64, 9i64]
-- }
-- structure { Screma 1 }

def main [k] [n]
         (indexes: [k]i64)
         (values: [k]i64)
         (array: *[n]i64) : [n]i64 =
  let values' = map2 (+) indexes values
  let array' = scatter array indexes values'
  in array'
