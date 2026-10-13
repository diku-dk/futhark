-- ==
-- tags { no_webgpu }
-- input {
--    [1.0, 2.0, 3.0, 4.0, 5.0]
-- }
-- output {
--    [2.0, 3.0, 4.0, 5.0, 0.0]
-- }
def main [m] (result: [m]f64) : []f64 =
  -- 0 <= i < m AND 0 <= j < n
  tabulate m (\j ->
                if j < m - 1
                then result[j + 1]
                else 0.0)
