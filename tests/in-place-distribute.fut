-- Extraction from generic pricer.  Uses shape declarations in ways
-- that were at one point problematic.
--
-- ==
-- input {
--   [1.0, 4.0, 7.0, 10.0, 13.0]
-- }
-- output {
--   [[1.000000, 1.000000, 1.000000, 1.000000, 1.000000], [4.000000,
--   16.000000, 256.000000, 65536.000000, 4294967296.000000], [7.000000,
--   49.000000, 2401.000000, 5764801.000000, 33232930569601.000000],
--   [10.000000, 100.000000, 10000.000000, 100000000.000000,
--   10000000000000000.000000], [13.000000, 169.000000, 28561.000000,
--   815730721.000000, 665416609183179904.000000]]
-- }

def seqloop (num_dates: i64) (gauss: f64) : [num_dates]f64 =
  let bbrow = replicate num_dates 0.0f64
  let bbrow[0] = gauss
  in loop (bbrow) for i in map (+ 1) (iota (num_dates - 1)) do
       let bbrow[i] = bbrow[i - 1] * bbrow[i - 1]
       in bbrow

def main [num_dates] (gausses: [num_dates]f64) : [][]f64 =
  map (seqloop (num_dates)) gausses
