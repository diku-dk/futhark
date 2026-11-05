-- The results of a map must not alias its input, even when either result may
-- be the parameter unchanged.
-- ==
-- random input { 0i64 [2][3][4]f32 } auto output
-- random input { 1i64 [2][3][4]f32 } auto output

entry main (c: i64) (xsss: [][][]f32) =
  let r =
    #[flattening(only_inner)]
    map (\xss ->
           if c != 0
           then (map (map (+ 1)) xss, xss)
           else (xss, map (map (+ 2)) xss))
        xsss
  in unzip r
