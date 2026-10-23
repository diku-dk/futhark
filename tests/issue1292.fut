-- ==
-- input {
-- [1i64,2i64,3i64,4i64,5i64]
-- [1.0f32,2.0f32,3.0f32,4.0f32,5.0f32]
-- [1.0f32,2.0f32,3.0f32,4.0f32,5.0f32]
-- }
-- auto output

def main [n] (is: [n]i64) (vs: [n]f32) (xs: [n]f32) : [n]f32 =
  let xs' = map2 (*) vs xs
  let vs' = map2 (*) vs xs'
  let ys = scatter xs' is vs'
  in ys
