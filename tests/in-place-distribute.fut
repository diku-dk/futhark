-- Extraction from generic pricer.  Uses shape declarations in ways
-- that were at one point problematic.
--
-- ==
-- input {
--   [1.0f32, 4.0f32, 7.0f32, 10.0f32, 13.0f32]
-- }
-- output {
--   [[1.000000f32, 1.000000f32, 1.000000f32, 1.000000f32, 1.000000f32], [4.000000f32,
--   16.000000f32, 256.000000f32, 65536.000000f32, 4294967296.000000f32], [7.000000f32,
--   49.000000f32, 2401.000000f32, 5764801.000000f32, 33232930569601.000000f32],
--   [10.000000f32, 100.000000f32, 10000.000000f32, 100000000.000000f32,
--   10000000000000000.000000f32], [13.000000f32, 169.000000f32, 28561.000000f32,
--   815730721.000000f32, 665416609183179904.000000f32]]
-- }

def seqloop (num_dates: i64) (gauss: f32) : [num_dates]f32 =
  let bbrow = replicate num_dates 0.0f32
  let bbrow[0] = gauss
  in loop (bbrow) for i in map (+ 1) (iota (num_dates - 1)) do
       let bbrow[i] = bbrow[i - 1] * bbrow[i - 1]
       in bbrow

def main [num_dates] (gausses: [num_dates]f32) : [][]f32 =
  map (seqloop (num_dates)) gausses
