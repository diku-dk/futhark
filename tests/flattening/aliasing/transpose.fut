-- The result of a map must not alias its input, even when the body may return
-- a transposition of its parameter.
-- ==
-- random input { 0i64 [2][3][3]f32 } auto output
-- random input { 1i64 [2][3][3]f32 } auto output

entry main [n] (c: i64) (xsss: [][n][n]f32) =
  #[flattening(only_inner)]
  map (\xss -> if c != 0 then map (map (+ 1)) xss else transpose xss) xsss
