-- The result of an inner map is fresh, so it can be consumed while its input
-- is still live, even when its body may return a view of its parameter.
-- ==
-- random input { 0i64 [2][3][4]f32 } auto output
-- random input { 1i64 [2][3][4]f32 } auto output

entry main (c: i64) (xsss: [][][]f32) =
  let r =
    #[flattening(only_inner)]
    map (\xss ->
           let ys = map (\xs -> if c != 0 then map (+ f32.sum xs) xs else reverse xs) xss
           let ys[0, 0] = 0
           in (ys, xss))
        xsss
  in unzip r
