-- ==
-- input {
--    [1.0, 2.0, 3.0, 4.0, 5.0]
-- }
-- output {
--    [2.0, 3.0, 4.0, 5.0, 0.0]
-- }
fun main(result:  [m]f64 ): []f64 =
  -- 0 <= i < m AND 0 <= j < n
  map  (\(j: int): f64  ->
            if j < (m-1)
            then unsafe result[j+1]
            else 0.0
      ) (iota(m) )
