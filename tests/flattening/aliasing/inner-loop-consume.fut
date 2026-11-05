-- The result of an inner map is fresh, so a loop can consume it while its
-- input is still live, even when its body may return its parameter.
-- ==
-- random input { 0i64 [2][3][4]f32 } auto output
-- random input { 1i64 [2][3][4]f32 } auto output

entry main (c: i64) (xsss: [][][]f32) =
  let r =
    #[flattening(only_inner)]
    map (\xss ->
           let ys = map (\xs -> if c != 0 then map (+ f32.sum xs) xs else xs) xss
           let ys = loop ys for i < 2 do ys with [i, 0] = 0
           in (ys, xss))
        xsss
  in unzip r
