-- Test that map-scatter fusion and scatter-scatter fusion work together.
-- ==
-- input {
--   [0i64, 1i64, 3i64]
--   [3i64, 2i64, 4i64, 6i64, 9i64, 14i64]
--   [13i64, 12i64, 14i64, 16i64, 19i64, 114i64]
-- }
-- output {
--   [3i64, 3i64, 4i64, 6i64, 6i64, 14i64]
--   [13i64, 12i64, 4i64, 5i64, 19i64, 7i64]
-- }
-- structure { Screma 1 }

def main [k] [n]
         (numbers: [k]i64)
         (array0: *[n]i64)
         (array1: *[n]i64) : ([n]i64, [n]i64) =
  let indexes0 = map (+ 1) numbers
  let indexes1 = map (+ 2) numbers
  let values0 = map (+ 3) numbers
  let values1 = map (+ 4) numbers
  let array0' = scatter array0 indexes0 values0
  let array1' = scatter array1 indexes1 values1
  in (array0', array1')
