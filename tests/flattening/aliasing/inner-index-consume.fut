-- The result of an inner map is fresh, so it can be consumed while its input
-- is still live, even when its body may return a row of its parameter.
-- ==
-- random input { 0i64 [2][3][2][4]f32 } auto output
-- random input { 1i64 [2][3][2][4]f32 } auto output

entry main [m] [n] (c: i64) (xssss: [][m][2][n]f32) =
  let r =
    #[flattening(only_inner)]
    map (\xsss ->
           let yss = map (\xss -> if c != 0 then map (+ 1) xss[0] else xss[1]) xsss
           let yss[0, 0] = 0
           in (yss, xsss))
        xssss
  in unzip r
