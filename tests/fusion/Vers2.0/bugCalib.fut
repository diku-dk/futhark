-- ==
-- input {
--    [1.0f32, 2.0f32, 3.0f32, 4.0f32, 5.0f32]
-- }
-- output {
--    [2.0f32, 3.0f32, 4.0f32, 5.0f32, 0.0f32]
-- }
def main [m] (result: [m]f32) : []f32 =
  -- 0 <= i < m AND 0 <= j < n
  tabulate m (\j ->
                if j < m - 1
                then result[j + 1]
                else 0.0)
