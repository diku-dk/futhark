-- ==
-- input {
--    [1.0, 2.0, 3.0, 4.0, 5.0]
-- }
-- output {
--    [2.0, 3.0, 4.0, 5.0, 0.0]
-- }
fun []f64 main( [m]f64 result ) =
  -- 0 <= i < m AND 0 <= j < n
  map ( fn f64 (int j) =>
            if j < (m-1)
            then unsafe result[j+1]
            else 0.0
      , iota(m) )
