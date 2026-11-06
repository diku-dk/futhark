-- The result of a map must not alias its input, even when the body may return
-- a reshaping of its parameter.
-- ==
-- random input { 0i64 [2][3][4]f32 } auto output
-- random input { 1i64 [2][3][4]f32 } auto output

entry main [m] [n] (c: i64) (xsss: [][m][n]f32) =
  #[flattening(only_inner)]
  map (\xss -> if c != 0 then map (+ 1) (flatten xss) else flatten xss) xsss
