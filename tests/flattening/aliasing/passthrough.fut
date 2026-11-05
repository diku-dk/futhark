-- The result of a map must not alias its input, even when the body may return
-- its parameter unchanged.
-- ==
-- random input { 0i64 [2][3][4]f32 } auto output
-- random input { 1i64 [2][3][4]f32 } auto output

entry main (c: i64) (xsss: [][][]f32) =
  #[flattening(only_inner)]
  map (\xss ->
         if c != 0
         then map (\xs -> let s = f32.sum xs in map (+ s) xs) xss
         else xss)
      xsss
