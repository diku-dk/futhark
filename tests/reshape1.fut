-- ==
-- input {
--   [1i64,2i64,3i64,4i64,5i64,6i64,7i64,8i64,9i64]
-- }
-- output {
--   [[1i64, 2i64, 3i64], [4i64, 5i64, 6i64], [7i64, 8i64, 9i64]]
-- }
-- input { [1i64,2i64,3i64] }
-- error: (3)*cannot match shape.*\[1\]i64

def intsqrt (x: i64) : i64 =
  i64.f32 (f32.sqrt (f32.i64 (x)))

def main [n] (a: [n]i64) : [][]i64 =
  unflatten (a :> [intsqrt n * intsqrt n]i64)
