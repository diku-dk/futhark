-- A better name would be 'take'...
-- ==
-- input {
--   3
--   [5.0,1.0,2.0,5.0]
-- }
-- output {
--   [5.000000, 1.000000, 2.000000]
--   [5.000000]
-- }
let main(n: i32, a: []f64): ([]f64, []f64) =
  split (n) a
