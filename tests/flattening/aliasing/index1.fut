-- The result of a map must not alias its input, even when the body may return
-- a row of its parameter.
-- ==
-- random input { 0i64 [2][2][3][4]f32 } auto output
-- random input { 1i64 [2][2][3][4]f32 } auto output

entry main [m] [n] [k] (c: i64) (xssss: [][m][n][k]f32) =
  #[flattening(only_inner)]
  map (\xsss -> if c != 0 then map (map (+ 1)) xsss[0] else xsss[1]) xssss
