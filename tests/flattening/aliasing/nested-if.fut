-- The result of a map must not alias its input, even when the parameter is
-- returned from a nested branch.
-- ==
-- random input { 0i64 0i64 [2][3][4]f32 } auto output
-- random input { 1i64 0i64 [2][3][4]f32 } auto output
-- random input { 1i64 1i64 [2][3][4]f32 } auto output

entry main (c: i64) (d: i64) (xsss: [][][]f32) =
  #[flattening(only_inner)]
  map (\xss ->
         if c != 0
         then if d != 0 then xss else map (map (+ 1)) xss
         else map (map (+ 2)) xss)
      xsss
