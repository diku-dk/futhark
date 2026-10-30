-- Simple rank-1 two-dimensional stencil computation.  Eventually
-- smooths out all differences.
-- ==
-- input {
-- 0
-- [[1.0f32,2.0f32,3.0f32],
--  [4.0f32,5.0f32,6.0f32],
--  [7.0f32,8.0f32,9.0f32]]
-- }
-- output {
-- [[1.0f32,2.0f32,3.0f32],
--  [4.0f32,5.0f32,6.0f32],
--  [7.0f32,8.0f32,9.0f32]]
-- }
-- input {
-- 1
-- [[1.0f32,2.0f32,3.0f32],
--  [4.0f32,5.0f32,6.0f32],
--  [7.0f32,8.0f32,9.0f32]]
-- }
-- output {
-- [[1.8f32, 2.6000000000000005f32, 3.4000000000000004f32],
-- [4.2f32, 5.0f32, 5.800000000000001f32],
-- [6.6000000000000005f32, 7.4f32, 8.2f32]]
-- }
-- input {
-- 2
-- [[1.0f32,2.0f32,3.0f32],
--  [4.0f32,5.0f32,6.0f32],
--  [7.0f32,8.0f32,9.0f32]]
-- }
-- output {
-- [[2.44f32, 3.0800000000000005f32, 3.7200000000000006f32],
--  [4.36f32, 5.0f32, 5.640000000000001f32],
--  [6.280000000000001f32, 6.920000000000001f32, 7.56f32]]
-- }

def main [n] [m] (num_iterations: i32) (a: [n][m]f32) : [][]f32 =
  loop (a) for i < num_iterations do
    map (\i ->
           map (\j ->
                  let center = a[i, j]
                  let north = if i == 0 then center else a[i - 1, j]
                  let east = if j == m - 1 then center else a[i, j + 1]
                  let south = if i == n - 1 then center else a[i + 1, j]
                  let west = if j == 0 then center else a[i, j - 1]
                  let factor = 1.0 / 5.0
                  in factor * center
                     + factor * north
                     + factor * east
                     + factor * south
                     + factor * west)
               (iota (m)))
        (iota (n))
