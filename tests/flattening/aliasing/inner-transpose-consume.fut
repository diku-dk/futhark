-- The result of an inner map is fresh, so it can be consumed while its input
-- is still live, even when its body may return a transposition of its
-- parameter.
-- ==
-- random input { 0i64 [2][2][3][3]f32 } auto output
-- random input { 1i64 [2][2][3][3]f32 } auto output

entry main [m] [n] (c: i64) (xssss: [][m][n][n]f32) =
  let r =
    #[flattening(only_inner)]
    map (\xsss ->
           let yss = map (\xss -> if c != 0 then map (map (+ 1)) xss else transpose xss) xsss
           let yss[0, 0, 0] = 0
           in (yss, xsss))
        xssss
  in unzip r
