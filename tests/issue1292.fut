-- ==
-- input {
-- [1i64,2i64,3i64,4i64,5i64]
-- [1.0,2.0,3.0,4.0,5.0]
-- [1.0,2.0,3.0,4.0,5.0]
-- }
-- auto output

def main [n] (is: [n]i64) (vs: [n]f64) (xs: [n]f64) : [n]f64 =
  let xs' = map2 (*) vs xs
  let vs' = map2 (*) vs xs'
  let ys = scatter xs' is vs'
  in ys
