-- Simple rank-1 one-dimensional stencil computation.  Eventually
-- smooths out all differences.
--
-- ==
-- input { 1i64 [1.0f32, 2.0f32, 3.0f32, 4.0f32, 5.0f32, 6.0f32] }
-- output { [1.3333333333333333f32, 2.0f32, 3.0f32, 3.9999999999999996f32, 5.0f32, 5.666666666666666f32] }
-- input { 2i64 [1.0f32, 2.0f32, 3.0f32, 4.0f32, 5.0f32, 6.0f32] }
-- output {
--   [1.5555555555555554f32,
--    2.111111111111111f32,
--    2.9999999999999996f32,
--    3.9999999999999996f32,
--    4.888888888888888f32,
--    5.444444444444444f32] }

def main [n] (num_iterations: i64) (a: [n]f32) : []f32 =
  loop (a) for i < num_iterations do
    map (\(i: i64) : f32 ->
           let x = if i == 0 then a[i] else a[i - 1]
           let y = a[i]
           let z = if i == n - 1 then a[i] else a[i + 1]
           let factor = 1.0 / 3.0
           in factor * x + factor * y + factor * z)
        (iota (n))
